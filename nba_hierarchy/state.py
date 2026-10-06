"""Pre-game state: what was known about each player before a given game.

Every feature here is computed from games strictly before the row it sits on.
The idiom throughout is

    pl.col(x).ewm_mean(...).shift(1).forward_fill().over("player_id")

which is load-bearing and not interchangeable with a plain shift. `ewm_mean`
emits null on a null input row rather than carrying its previous value, so on a
panel where most players are absent for part of the season a bare `.shift(1)`
would blank the feature after every absence. Shifting first and then forward
filling yields the last state from a game the player actually played, strictly
before the current row.

The same function serves training and the daily run. `state_as_of` appends
virtual rows at the target date and feeds them through `add_pre_game_state`
unchanged, so the serving path cannot drift from the training path; the test
suite asserts the two agree on real dates.
"""

from __future__ import annotations

from datetime import date

import polars as pl

from .config import EWM_HALF_LIVES, RATE_STATS, ROLL_WINDOWS, SERVING_GAME_ID
from .data import STATUS_COACH

# Half-life used to rank a team's players into a depth chart.
DEPTH_ANCHOR_HL = 8

PRESENCE_PENDING = "PENDING"


def _prior_ewm(col: str, half_life: int) -> pl.Expr:
    """EWMA of `col` over games played strictly before this row."""
    return (
        pl.col(col)
        .ewm_mean(half_life=half_life, ignore_nulls=True)
        .shift(1)
        .forward_fill()
        .over("player_id")
    )


def _prior_roll(expr: pl.Expr, window: int) -> pl.Expr:
    """Mean of `expr` over the player's last `window` team games, excluding this one."""
    return (
        expr.cast(pl.Float64)
        .rolling_mean(window_size=window, min_samples=1)
        .shift(1)
        .over("player_id")
    )


def add_pre_game_state(
    panel: pl.DataFrame,
    half_lives: tuple[int, ...] = EWM_HALF_LIVES,
    windows: tuple[int, ...] = ROLL_WINDOWS,
    rate_stats: tuple[str, ...] = RATE_STATS,
) -> pl.DataFrame:
    """Attach pre-game features to every row of the panel.

    Expects the panel sorted by (player_id, game_date, game_id); `.over()`
    windows read each player's games in that order.
    """
    panel = panel.sort(["player_id", "game_date", "game_id"])

    # Minutes with an absence counted as zero. This is the depth signal: a
    # player who misses six weeks should decay down the rotation, which a
    # played-only average would never show.
    panel = panel.with_columns(
        min_or_zero=pl.col("min").fill_null(0.0),
        min_played=pl.when(pl.col("played")).then(pl.col("min")).otherwise(None),
    )

    feats: list[pl.Expr] = []
    for hl in half_lives:
        feats.append(
            pl.col("min_or_zero")
            .ewm_mean(half_life=hl, ignore_nulls=True)
            .shift(1)
            .over("player_id")
            .alias(f"ewm_min_{hl}")
        )
        feats.append(_prior_ewm("min_played", hl).alias(f"ewm_min_played_{hl}"))
        feats.extend(
            _prior_ewm(stat, hl).alias(f"ewm_{stat}_{hl}") for stat in rate_stats
        )

    # Availability and role rates over recent team games.
    for w in windows:
        feats.append(_prior_roll(pl.col("played"), w).alias(f"play_rate_{w}"))
        feats.append(_prior_roll(pl.col("started"), w).alias(f"start_rate_{w}"))
        feats.append(_prior_roll(pl.col("with_team"), w).alias(f"with_team_rate_{w}"))
        feats.append(
            _prior_roll(pl.col("presence") == STATUS_COACH, w).alias(f"coach_dnp_rate_{w}")
        )

    # Counters and recency. `team_games_prior` counts panel rows, i.e. games the
    # player's team played while he was on the roster.
    played_idx = pl.when(pl.col("played")).then(pl.col("_idx")).otherwise(None)
    played_date = pl.when(pl.col("played")).then(pl.col("game_date")).otherwise(None)
    feats.extend(
        [
            pl.col("_idx").alias("team_games_prior"),
            pl.col("played")
            .cast(pl.Int32)
            .cum_sum()
            .shift(1)
            .fill_null(0)
            .over("player_id")
            .alias("career_games_prior"),
            pl.col("played")
            .cast(pl.Int32)
            .cum_sum()
            .shift(1)
            .fill_null(0)
            .over(["player_id", "season"])
            .alias("season_games_prior"),
            (pl.col("game_date") - pl.col("game_date").shift(1).over("player_id"))
            .dt.total_days()
            .alias("days_rest"),
            (pl.col("game_date") - played_date.shift(1).forward_fill().over("player_id"))
            .dt.total_days()
            .alias("days_since_played"),
            # Team games the player missed between his last appearance and
            # this one, counting neither end: 0 means he played the previous
            # game. Subtracting 1 matters -- without it the current game, which
            # has not been played yet, is counted as already missed.
            (
                pl.col("_idx")
                - played_idx.shift(1).forward_fill().over("player_id")
                - 1
            ).alias("team_games_missed"),
            # Consecutive games the player was not with the team immediately
            # before this one. Unlike `absence_run_full` this looks only
            # backwards, so it is safe as a feature. Filling the "last game he
            # was present for" with -1 covers a player who has never been
            # present: every prior game was then an absence, and on his very
            # first row there are no prior games and so no prior absences.
            (
                pl.col("_idx")
                - 1
                - pl.when(pl.col("with_team"))
                .then(pl.col("_idx"))
                .otherwise(None)
                .shift(1)
                .forward_fill()
                .over("player_id")
                .fill_null(-1)
            )
            .clip(0)
            .alias("absent_streak_prior"),
        ]
    )

    panel = panel.with_columns(
        _idx=pl.int_range(pl.len(), dtype=pl.Int32).over("player_id")
    ).with_columns(feats)

    # Depth rank within the team for this game, from trailing minutes. A player
    # with no history ranks last rather than null.
    anchor = f"ewm_min_{DEPTH_ANCHOR_HL}"
    panel = panel.with_columns(
        depth_rank=pl.col(anchor)
        .fill_null(0.0)
        .rank("ordinal", descending=True)
        .over(["game_id", "team_id"])
        .cast(pl.Int32),
        team_size=pl.len().over(["game_id", "team_id"]).cast(pl.Int32),
    )
    return panel.drop("_idx")


def candidate_roster(
    panel: pl.DataFrame, as_of_date: date, lookback_games: int = 10
) -> pl.DataFrame:
    """Players to predict for: anyone on a team's roster in its recent games.

    Membership is read from the panel rather than the season roster table, which
    is both incomplete and undated.

    Exactly one row per player. A player traded inside the lookback window is on
    both teams' recent rosters, and emitting a virtual row for each would give
    him two rows on the same date -- which then corrupt each other, since the
    second sees the first as a prior game. His current team is the one he last
    appeared for.
    """
    prior = panel.filter(pl.col("game_date") < as_of_date)
    recent_games = (
        prior.select("team_id", "game_id", "game_date")
        .unique()
        .sort(["team_id", "game_date", "game_id"], descending=[False, True, True])
        .with_columns(rn=pl.int_range(pl.len()).over("team_id"))
        .filter(pl.col("rn") < lookback_games)
        .select("team_id", "game_id")
    )
    on_recent_roster = (
        prior.join(recent_games, on=["team_id", "game_id"], how="inner")
        .select("player_id")
        .unique()
    )
    current_team = (
        prior.sort(["player_id", "game_date", "game_id"])
        .group_by("player_id")
        .tail(1)
        .select("season", "team_id", "team_abbreviation", "player_id", "player_name")
    )
    return on_recent_roster.join(current_team, on="player_id", how="inner")


def append_serving_rows(
    panel: pl.DataFrame,
    as_of_date: date,
    lookback_games: int = 10,
) -> pl.DataFrame:
    """Panel history before `as_of_date`, plus a virtual row per rostered player.

    Returned unfiltered so that every later layer -- positions, availability,
    absorption -- can run over the same frame the training build sees, with the
    target date's rows picked out only at the end. Filtering here instead would
    strip each player's history and silently null every trailing feature.
    """
    prior = panel.filter(pl.col("game_date") < as_of_date)
    candidates = candidate_roster(panel, as_of_date, lookback_games)

    virtual = candidates.with_columns(
        game_id=pl.lit(SERVING_GAME_ID, dtype=pl.Int64),
        game_date=pl.lit(as_of_date, dtype=pl.Date),
        played=pl.lit(False),
        started=pl.lit(False),
        with_team=pl.lit(True),
        presence=pl.lit(PRESENCE_PENDING),
        absence_run_full=pl.lit(0, dtype=pl.UInt32),
    )
    for col, dtype in panel.schema.items():
        if col not in virtual.columns:
            virtual = virtual.with_columns(pl.lit(None, dtype=dtype).alias(col))
    return pl.concat([prior, virtual.select(panel.columns)], how="vertical")


def serving_rows(df: pl.DataFrame) -> pl.DataFrame:
    """The virtual rows added by `append_serving_rows`."""
    return df.filter(pl.col("game_id") == SERVING_GAME_ID)


def state_as_of(
    panel: pl.DataFrame,
    as_of_date: date,
    lookback_games: int = 10,
    **kwargs,
) -> pl.DataFrame:
    """Layer 1 state for every rostered player as of `as_of_date`.

    Virtual rows are run through `add_pre_game_state` unchanged, so serving and
    training share one code path.
    """
    combined = append_serving_rows(panel, as_of_date, lookback_games)
    return serving_rows(add_pre_game_state(combined, **kwargs))

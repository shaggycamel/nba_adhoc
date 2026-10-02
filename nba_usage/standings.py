"""Standings, and the incentives they create.

A team's league position changes what it wants from a game, and that is not
visible anywhere in a player's own history. Two regimes matter most, and
both only bite late in the season: a team chasing a playoff place hurries a
starter back and leans on its best players, while a team out of the race
shuts them down and hands the minutes to whoever it wants to look at. The
same injury therefore produces a different absence and a different
replacement workload depending on where the team sits and when.

Everything is computed from games already played, so a row sees only the
table as it stood before tip-off.
"""

from __future__ import annotations

import polars as pl

from .panel import DATA

GAMES_IN_SEASON = 82
PLAYOFF_CUTOFF = 6      # top six avoid the play-in
PLAY_IN_CUTOFF = 10     # seventh to tenth play in


def _results() -> pl.DataFrame:
    sched = (
        pl.read_parquet(DATA / "nba" / "league_game_schedule.parquet")
        .filter(pl.col("season_type") == "Regular Season")
        .select("season", "game_id", "game_date", pl.col("team").alias("team_abbreviation"), "team_winner")
    )
    return sched.with_columns(won=(pl.col("team_winner") == pl.col("team_abbreviation")).cast(pl.Int32))


def team_record() -> pl.DataFrame:
    """Each team's record as it stood before each of its games."""
    r = _results().sort("season", "team_abbreviation", "game_date")
    return r.with_columns(
        wins_before=pl.col("won").cum_sum().shift(1).fill_null(0).over(["season", "team_abbreviation"]),
        games_before=pl.int_range(pl.len()).over(["season", "team_abbreviation"]).cast(pl.Int32),
    ).with_columns(
        win_pct=pl.when(pl.col("games_before") > 0)
        .then(pl.col("wins_before") / pl.col("games_before"))
        .otherwise(0.5),
        season_frac=pl.col("games_before") / GAMES_IN_SEASON,
        games_remaining=GAMES_IN_SEASON - pl.col("games_before"),
    ).select(
        "season", "game_id", "game_date", "team_abbreviation",
        "win_pct", "games_before", "season_frac", "games_remaining",
    )


def _conference() -> pl.DataFrame:
    teams = pl.read_parquet(DATA / "nba" / "teams.parquet")
    conf_col = next(c for c in teams.columns if "conf" in c.lower())
    slug = next(c for c in teams.columns if c.lower() in {"team_slug", "team_abbreviation"})
    return teams.select(pl.col(slug).alias("team_abbreviation"), pl.col(conf_col).alias("conference")).unique()


def standings_features() -> pl.DataFrame:
    """Conference position, distance from the cut lines, and the incentives.

    Ranking has to be done on a daily grid, not on game rows: teams play on
    different nights, so a team's position depends on every other team's
    record that morning, including the ones not playing.
    """
    rec = team_record().join(_conference(), on="team_abbreviation", how="left")

    # Every team's record carried forward to every date its conference
    # played, so all 15 are comparable on the same day.
    dates = rec.select("season", "game_date").unique()
    teams = rec.select("season", "team_abbreviation", "conference").unique()
    grid = dates.join(teams, on="season", how="inner").sort("game_date")

    standing = (
        grid.join_asof(
            rec.select("season", "team_abbreviation", "game_date", "win_pct").sort("game_date"),
            on="game_date",
            by=["season", "team_abbreviation"],
            strategy="backward",
        )
        .with_columns(win_pct=pl.col("win_pct").fill_null(0.5))
        .with_columns(
            conf_rank=pl.col("win_pct")
            .rank("ordinal", descending=True)
            .over(["season", "game_date", "conference"])
            .cast(pl.Int32)
        )
        .select("season", "game_date", "team_abbreviation", "conf_rank")
    )

    ranked = rec.join(standing, on=["season", "game_date", "team_abbreviation"], how="left")

    return ranked.with_columns(
        above_playoff_line=pl.col("conf_rank") <= PLAYOFF_CUTOFF,
        in_play_in=(pl.col("conf_rank") > PLAYOFF_CUTOFF) & (pl.col("conf_rank") <= PLAY_IN_CUTOFF),
        out_of_race=pl.col("conf_rank") > PLAY_IN_CUTOFF,
    ).with_columns(
        # The incentives only bite late, so each is the state times how far
        # through the season the team is.
        tank_pressure=pl.col("out_of_race").cast(pl.Float64) * pl.col("season_frac"),
        push_pressure=pl.col("in_play_in").cast(pl.Float64) * pl.col("season_frac"),
        contend_pressure=pl.col("above_playoff_line").cast(pl.Float64) * pl.col("season_frac"),
        locked_in=((pl.col("conf_rank") <= 2).cast(pl.Float64)) * pl.col("season_frac"),
    ).select("game_id", "team_abbreviation", *STANDINGS_COLS)


STANDINGS_COLS = [
    "win_pct",
    "conf_rank",
    "season_frac",
    "games_remaining",
    "above_playoff_line",
    "in_play_in",
    "out_of_race",
    "tank_pressure",
    "push_pressure",
    "contend_pressure",
    "locked_in",
]

"""Injury-report features: the player's own status and usage vacated by absent teammates.

The report is published before tip-off, so it is legitimate pre-game
information. Two repairs are needed first: `game_id` is missing for most of
2021-22 and `nba_id` for about 18% of rows, so both are recovered by joining
on the date/team and on the season-aware name map in `data/util/`.

Coverage starts 2021-10-19; nothing before that has an injury report.
"""

from __future__ import annotations

import polars as pl

from .panel import DATA

INJURY_START = "2021-10-19"

# Ordinal availability: higher means more likely to be limited or absent.
STATUS_RANK = {
    "Available": 0,
    "Probable": 1,
    "Questionable": 2,
    "Doubtful": 3,
    "Out": 4,
}


def _name_map() -> pl.DataFrame:
    """Season-aware NBA name -> nba player id, from the util crosswalk."""
    return (
        pl.read_parquet(DATA / "util" / "active_player_vw.parquet")
        .filter(pl.col("platform") == "nba")
        .select(
            "season",
            pl.col("source_name").alias("player_name"),
            pl.col("source_id").alias("mapped_id"),
        )
        .unique(subset=["season", "player_name"])
    )


def load_injuries() -> pl.DataFrame:
    """One row per (game_id, player): the last status the report carried.

    A player-game can appear several times as the report is updated through
    the day; the final row is the closest thing to the status known at
    tip-off, so that is the one kept.
    """
    sched = (
        pl.read_parquet(DATA / "nba" / "league_game_schedule.parquet")
        .select("game_id", "game_date", "season", pl.col("team").alias("team_abbreviation"))
    )
    inj = (
        pl.read_parquet(DATA / "nba" / "injuries.parquet")
        .rename({"game_id": "reported_game_id", "team_slug": "team_abbreviation"})
        .drop("team")
    )

    # Recover game_id (and season) from the date/team pair the report always has.
    inj = inj.join(sched, on=["game_date", "team_abbreviation"], how="inner")

    # Recover the missing nba_id from the season's name map.
    inj = (
        inj.join(_name_map(), on=["season", "player_name"], how="left")
        .with_columns(player_id=pl.coalesce("nba_id", "mapped_id"))
        .drop("mapped_id", "nba_id", "reported_game_id")
    )

    # Keep the last report line per player-game, and rank the status.
    inj = (
        inj.with_row_index("_row")
        .sort("_row")
        .group_by(["game_id", "player_id"], maintain_order=True)
        .last()
        .drop("_row")
        .with_columns(
            status_rank=pl.col("status").replace_strict(STATUS_RANK, default=None, return_dtype=pl.Int8),
            is_out=pl.col("status").str.strip_chars() == "Out",
        )
    )
    return inj.filter(pl.col("player_id").is_not_null())


def player_state(played: pl.DataFrame) -> pl.DataFrame:
    """Each player's role level as of the end of each game they played.

    Used to value an absent player: what usage and minutes is the team losing?
    Unlike the model features these windows *include* the row's own game,
    because they are read as-of a strictly earlier date.
    """
    return (
        played.sort("player_id", "game_date")
        .with_columns(
            state_usg=pl.col("usg_pct").rolling_mean(10, min_samples=1).over("player_id"),
            state_min=pl.col("min").rolling_mean(10, min_samples=1).over("player_id"),
        )
        .select("player_id", "game_date", "state_usg", "state_min")
    )


def absence_features(panel: pl.DataFrame, played: pl.DataFrame) -> pl.DataFrame:
    """Per (game_id, team): what the injury report says the team is missing.

    `vacated_usg_min` is the headline quantity — the usage-weighted minutes of
    the players ruled out, i.e. the share of the team's shot-creating load
    that has to be absorbed by whoever does play.
    """
    inj = load_injuries()
    state = player_state(played)

    # Value each reported player by their role as of their last prior game.
    valued = (
        inj.sort(["player_id", "game_date"])
        .join_asof(
            state.sort(["player_id", "game_date"]),
            on="game_date",
            by="player_id",
            strategy="backward",
            allow_exact_matches=False,
        )
        .with_columns(
            state_usg=pl.col("state_usg").fill_null(0.0),
            state_min=pl.col("state_min").fill_null(0.0),
        )
    )

    out = valued.filter(pl.col("is_out"))
    team_out = out.group_by(["game_id", "team_abbreviation"]).agg(
        n_teammates_out=pl.len(),
        vacated_usg=pl.col("state_usg").sum(),
        vacated_min=pl.col("state_min").sum(),
        vacated_usg_min=(pl.col("state_usg") * pl.col("state_min")).sum(),
        vacated_top_usg=pl.col("state_usg").max().fill_null(0.0),
        vacated_starter_min=(pl.col("state_min") >= 24).sum(),
    )

    questionable = valued.filter(pl.col("status_rank").is_between(1, 3))
    team_q = questionable.group_by(["game_id", "team_abbreviation"]).agg(
        n_teammates_questionable=pl.len(),
        questionable_usg=pl.col("state_usg").sum(),
    )

    own = valued.select(
        "game_id",
        "player_id",
        pl.col("status_rank").alias("own_status_rank"),
        pl.col("is_out").alias("own_is_out"),
    )

    # Team totals give the absences a denominator: losing 40 usage-minutes
    # means more on a thin roster than a deep one.
    team_totals = (
        played.join(state, on=["player_id", "game_date"], how="left")
        .group_by(["game_id", "team_abbreviation"])
        .agg(team_state_usg_min=(pl.col("state_usg") * pl.col("state_min")).sum())
    )

    feats = (
        panel.select("game_id", "team_abbreviation", "player_id")
        .join(team_out, on=["game_id", "team_abbreviation"], how="left")
        .join(team_q, on=["game_id", "team_abbreviation"], how="left")
        .join(team_totals, on=["game_id", "team_abbreviation"], how="left")
        .join(own, on=["game_id", "player_id"], how="left")
        .with_columns(
            [pl.col(c).fill_null(0) for c in
             ["n_teammates_out", "vacated_usg", "vacated_min", "vacated_usg_min",
              "vacated_top_usg", "vacated_starter_min", "n_teammates_questionable",
              "questionable_usg"]]
        )
        .with_columns(
            vacated_share=(
                pl.col("vacated_usg_min")
                / (pl.col("vacated_usg_min") + pl.col("team_state_usg_min")).replace(0, None)
            ).fill_null(0.0),
            own_status_rank=pl.col("own_status_rank").fill_null(0),
            own_is_out=pl.col("own_is_out").fill_null(False),
        )
    )
    return feats


ABSENCE_COLS = [
    "n_teammates_out",
    "vacated_usg",
    "vacated_min",
    "vacated_usg_min",
    "vacated_top_usg",
    "vacated_starter_min",
    "n_teammates_questionable",
    "questionable_usg",
    "vacated_share",
    "own_status_rank",
]

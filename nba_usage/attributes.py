"""Player biography and where the season has got to.

A player's own rolling history already encodes much of what height and
position would say: a seven-footer's rebound rate is visible without being
told their height. What history cannot supply is trajectory and schedule.
Age separates a 22-year-old whose recent form understates them from a
34-year-old whose recent form flatters him, and both respond differently to
a back-to-back. Draft pedigree says something about which young player a
coach trusts with vacated minutes before the evidence exists.
"""

from __future__ import annotations

import polars as pl

from .panel import DATA

UNDRAFTED_SLOT = 61  # one past the last pick, so undrafted sorts as least pedigree


def player_attributes() -> pl.DataFrame:
    """Static biography per player, cleaned into numbers.

    `player_info` only carries the two most recent seasons, but birth date,
    height and draft position do not change, so the latest record is used for
    every season the player appears in. Experience is derived from the season
    rather than taken from the row, since that one does change.
    """
    pi = pl.read_parquet(DATA / "nba" / "player_info.parquet").select(
        "season", "player_id", "birthdate", "height_cm", "weight_kg", "season_exp", "position", "draft_number"
    )
    pos = pl.col("position").fill_null("")
    latest = (
        pi.sort("season")
        .group_by("player_id")
        .last()
        .with_columns(
            birth=pl.col("birthdate").str.slice(0, 10).str.to_date(strict=False),
            draft_pick=pl.col("draft_number").cast(pl.Int32, strict=False).fill_null(UNDRAFTED_SLOT),
            is_guard=pos.str.contains("Guard"),
            is_forward=pos.str.contains("Forward"),
            is_center=pos.str.contains("Center"),
            # Experience as of the season the record came from, so it can be
            # rolled back for earlier ones.
            exp_asof=pl.col("season").str.slice(0, 4).cast(pl.Int32),
        )
        .select(
            "player_id", "birth", "height_cm", "weight_kg", "season_exp", "exp_asof",
            "draft_pick", "is_guard", "is_forward", "is_center",
        )
    )
    return latest


def add_attributes(frame: pl.DataFrame) -> pl.DataFrame:
    """Biography, age at tip-off, and how congested the schedule has been."""
    out = (
        frame.join(player_attributes(), on="player_id", how="left")
        .with_columns(
            age_years=((pl.col("game_date") - pl.col("birth")).dt.total_days() / 365.25),
            # Roll the recorded experience back to this season.
            season_exp=(
                pl.col("season_exp")
                - (pl.col("exp_asof") - pl.col("season").str.slice(0, 4).cast(pl.Int32))
            ).clip(0, None),
        )
        .drop("birth", "exp_asof")
    )

    # Schedule density: games this player's team has played in the previous
    # week, which drives rest decisions independently of a single day's rest.
    dens = (
        frame.select("team_abbreviation", "game_date")
        .unique()
        .sort("team_abbreviation", "game_date")
        .with_columns(
            games_last_7d=pl.col("game_date")
            .rolling_mean_by("game_date", window_size="7d", closed="left")
            .over("team_abbreviation")
            .is_not_null()
            .cast(pl.Int8)
        )
        .select("team_abbreviation", "game_date")
    )
    counts = (
        frame.select("team_abbreviation", "game_date")
        .unique()
        .sort("game_date")
        .rolling(index_column="game_date", period="7d", closed="left", group_by="team_abbreviation")
        .agg(games_last_7d=pl.len())
    )
    return out.join(counts, on=["team_abbreviation", "game_date"], how="left").with_columns(
        games_last_7d=pl.col("games_last_7d").fill_null(0)
    )


ATTRIBUTE_COLS = [
    "height_cm", "weight_kg", "season_exp", "draft_pick", "age_years",
    "is_guard", "is_forward", "is_center", "games_last_7d",
]

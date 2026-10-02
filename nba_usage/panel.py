"""Build the player-game panel that every experiment starts from.

One row per player-game. Everything here is either an identifier, the target,
or a fact known *after* tip-off; prior-game features live in `features.py` so
the leakage boundary stays in one place.
"""

from __future__ import annotations

from pathlib import Path

import polars as pl

DATA = Path(__file__).resolve().parent.parent / "data"

# Box-score columns worth keeping: the target, the volume stats a role profile
# is built from, and enough context to sanity-check a row.
_BOX_COLS = [
    "game_id",
    "team_id",
    "team_abbreviation",
    "player_id",
    "player_name",
    "start_position",
    "comment",
    "min",
    "fga",
    "fta",
    "tov",
    "pts",
    "ast",
    "reb",
    "poss",
    "usg_pct",
    "ast_pct",
    "tov_pct",
    "ts_pct",
    "pace",
]


def _schedule() -> pl.LazyFrame:
    """Game-level context, one row per (game_id, team)."""
    return (
        pl.scan_parquet(DATA / "nba" / "league_game_schedule.parquet")
        .select(
            "game_id",
            "game_date",
            "season",
            "season_type",
            "home",
            pl.col("team").alias("team_abbreviation"),
            pl.col("opponent"),
        )
    )


def load_panel(season_types: tuple[str, ...] = ("Regular Season",)) -> pl.DataFrame:
    """Player-game rows joined to their date, season and opponent.

    Keeps DNP rows (null `min`): a player who was dressed but did not play is
    still evidence about the rotation, and teammate-absence features need to
    know they were on the roster. Rows where the player appeared are flagged
    with `played`.
    """
    box = pl.scan_parquet(DATA / "nba" / "player_box_score.parquet").select(_BOX_COLS)
    sched = _schedule()

    panel = (
        box.join(sched, on=["game_id", "team_abbreviation"], how="inner")
        .filter(pl.col("season_type").is_in(season_types))
        .with_columns(
            played=(pl.col("min").fill_null(0) > 0) & pl.col("usg_pct").is_not_null(),
            dnp_reason=pl.col("comment").str.strip_chars().replace("", None),
        )
        .sort("player_id", "game_date", "game_id")
    )
    return panel.collect()


def add_game_order(panel: pl.DataFrame) -> pl.DataFrame:
    """Per-player ordering helpers: appearance counts and days of rest.

    Counts are of *played* games only and are shifted, so each row sees only
    what was true before tip-off.
    """
    return panel.with_columns(
        career_game_n=pl.col("played").cast(pl.Int32).cum_sum().shift(1).fill_null(0).over("player_id"),
        season_game_n=pl.col("played").cast(pl.Int32).cum_sum().shift(1).fill_null(0).over(["player_id", "season"]),
        days_rest=(
            pl.col("game_date")
            - pl.when(pl.col("played"))
            .then(pl.col("game_date"))
            .shift(1)
            .forward_fill()
            .over("player_id")
        ).dt.total_days(),
    )

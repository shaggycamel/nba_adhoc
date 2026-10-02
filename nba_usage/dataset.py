"""One entry point for the modelling frame, so every experiment starts identically."""

from __future__ import annotations

from pathlib import Path

import polars as pl

from .features import add_prior_features
from .injuries import absence_features
from .panel import add_game_order, load_panel


def build(cache: Path | None = None, rebuild: bool = False) -> pl.DataFrame:
    """Played player-games with lagged history and injury-report context."""
    if cache is not None and cache.exists() and not rebuild:
        return pl.read_parquet(cache)

    panel = add_game_order(load_panel())
    played = panel.filter(pl.col("played"))
    frame = add_prior_features(played).join(
        absence_features(panel, played),
        on=["game_id", "player_id", "team_abbreviation"],
        how="left",
    )
    if cache is not None:
        cache.parent.mkdir(parents=True, exist_ok=True)
        frame.write_parquet(cache)
    return frame

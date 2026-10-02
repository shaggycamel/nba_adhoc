"""One entry point for the modelling frame, so every experiment starts identically."""

from __future__ import annotations

from pathlib import Path

import polars as pl

from .features import add_prior_features
from .hierarchy import add_rotation_features
from .injuries import absence_features
from .panel import add_game_order, load_panel
from .roles import N_ROLES, assign_roles, role_absence_features
from .synergy import absorption_features


def build(cache: Path | None = None, rebuild: bool = False) -> pl.DataFrame:
    """Played player-games with lagged history and injury-report context."""
    if cache is not None and cache.exists() and not rebuild:
        return pl.read_parquet(cache)

    panel = add_game_order(load_panel())
    played = panel.filter(pl.col("played"))
    roles = assign_roles(panel, played)
    frame = (
        add_prior_features(played)
        .join(
            absence_features(panel, played),
            on=["game_id", "player_id", "team_abbreviation"],
            how="left",
        )
        .join(
            add_rotation_features(panel, played),
            on=["game_id", "player_id", "team_abbreviation"],
            how="left",
        )
        .join(
            absorption_features(panel),
            on=["game_id", "player_id"],
            how="left",
        )
        .join(
            role_absence_features(panel, played, roles),
            on=["game_id", "player_id", "team_abbreviation"],
            how="left",
        )
        # A player who played despite being ruled out has no rank among the
        # available; treat them as the bottom of the rotation.
        .with_columns(
            rotation_rank=pl.col("rotation_rank").fill_null(pl.col("rotation_size")),
            rotation_rank_norm=pl.col("rotation_rank_norm").fill_null(1.0),
        )
        # One-hot the role so the linear models can use it on equal terms.
        .with_columns(
            [
                (pl.col("role") == r).fill_null(False).alias(f"role_{r}")
                for r in range(N_ROLES)
            ]
        )
    )
    if cache is not None:
        cache.parent.mkdir(parents=True, exist_ok=True)
        frame.write_parquet(cache)
    return frame

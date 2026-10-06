"""Shared configuration for the team-hierarchy model."""

from __future__ import annotations

from datetime import date
from pathlib import Path

DATA_DIR = Path(__file__).resolve().parent.parent / "data"

# `nba/injuries` begins here. Layers 1-2 train on all available history
# (2009-10 onward); layers 3-4 are restricted to this era.
INJURY_ERA_START = date(2021, 10, 19)

# All Star games distort rate stats and pre-season rotations are not
# hierarchy-bearing, so both are excluded by default.
SEASON_TYPES: tuple[str, ...] = ("Regular Season",)

# EWMA half-lives in games: roughly "last week", "last month", "this season".
EWM_HALF_LIVES: tuple[int, ...] = (3, 8, 20)

# Rolling windows in team games, for availability and role rates.
ROLL_WINDOWS: tuple[int, ...] = (5, 10, 20)

# Rate stats carried in the pre-game state. Averaged over games the player
# actually played, since a DNP carries no rate information.
RATE_STATS: tuple[str, ...] = (
    "usg_pct",
    "ast_pct",
    "reb_pct",
    "oreb_pct",
    "dreb_pct",
    "ts_pct",
    "fg3_rate",
    "ftr",
    "fga36",
    "ast36",
    "reb36",
    "blk36",
    "stl36",
    "tov36",
)

# Sentinel game_id for the virtual rows used by the serving path.
SERVING_GAME_ID = -1

"""Filesystem layout and project-wide constants."""

from __future__ import annotations

from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
DATA = ROOT / "data"
NBA = DATA / "nba"
STATYX = DATA / "statyx"
UTIL = DATA / "util"

# Derived artefacts. Git-ignored alongside the rest of `data/`.
BUILD = DATA / "build"
REPORTS = ROOT / "reports"

INJURIES = NBA / "injuries.parquet"
SCHEDULE = NBA / "league_game_schedule.parquet"
BOX = NBA / "player_box_score.parquet"
TEAM_BOX = NBA / "team_box_score.parquet"
PLAYER_INFO = NBA / "player_info.parquet"
TEAMS = NBA / "teams.parquet"
ROSTER = NBA / "team_roster.parquet"
PLAYER_ID_MAP = UTIL / "player_id_map_vw.parquet"

# The injury report starts partway through 2021-22; earlier seasons have
# schedule and box-score rows but no availability information.
FIRST_SEASON = "2021-22"

# Time-based evaluation. Nothing is ever shuffled: a fold trains on whole
# seasons that finished before the season it scores.
SEASONS = ["2021-22", "2022-23", "2023-24", "2024-25", "2025-26"]
VALID_SEASONS = ["2024-25"]
TEST_SEASONS = ["2025-26"]
TRAIN_SEASONS = ["2021-22", "2022-23", "2023-24"]


def ensure_build() -> Path:
    BUILD.mkdir(parents=True, exist_ok=True)
    return BUILD


def ensure_reports() -> Path:
    REPORTS.mkdir(parents=True, exist_ok=True)
    return REPORTS

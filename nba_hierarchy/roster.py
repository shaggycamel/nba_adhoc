"""Build the roster-complete player panel: one row per team game per player.

The box score only carries rows for players the team listed that night, so an
absent player usually leaves no trace at all -- 92.7% of players the injury
report lists as Out have no box score row. A model asked who absorbs an absent
teammate's minutes cannot see the absence in that data.

This module reconstructs the missing rows. For each (season, team, player) it
takes a membership interval bounded by dated evidence -- box score appearances
and injury report entries -- and emits a row for every one of that team's games
inside the interval, tagged with how the player's presence is known.

What is and is not recovered:

* Interior gaps (a player who appears, disappears for two months, reappears)
  are recovered in every season, since both ends are observed.
* Leading and trailing gaps (injured before their first appearance, or out for
  the rest of the season) are recovered only from 2021-10-19, where the injury
  report extends the interval. Earlier seasons cannot distinguish "injured" from
  "not yet signed".
* A player waived and later re-signed by the same team inside one season gets
  one interval spanning the gap, so the time away reads as ABSENT_UNKNOWN.
  `absence_run` lets a downstream model discount implausibly long runs.
"""

from __future__ import annotations

from pathlib import Path

import polars as pl

from .config import DATA_DIR, SEASON_TYPES
from .data import load_player_games

# Presence on a team game, superseding the box score `status`.
PRESENCE_ABSENT_INJ = "ABSENT_INJ"
PRESENCE_ABSENT_UNKNOWN = "ABSENT_UNKNOWN"

# Injury report statuses, most to least severe. Only one status per player-date
# is kept; the table has no timestamp, so "most severe" stands in for "last
# report before tip-off". Layer 3 refines this.
_STATUS_SEVERITY = {
    "Out": 0,
    "Doubtful": 1,
    "Questionable": 2,
    "Probable": 3,
    "Available": 4,
}
# Statuses that count as an expected absence when no box score row exists.
_ABSENT_STATUSES = ("Out", "Doubtful")


def load_injury_reports(
    data_dir: Path = DATA_DIR, date_to_season: pl.DataFrame | None = None
) -> pl.DataFrame:
    """One row per (game_date, team, player) with a single resolved status."""
    teams = pl.read_parquet(data_dir / "nba" / "teams.parquet").select(
        "team_id", "team_slug"
    )
    inj = (
        pl.read_parquet(data_dir / "nba" / "injuries.parquet")
        .filter(pl.col("nba_id").is_not_null())
        .rename({"nba_id": "player_id"})
        .join(teams, on="team_slug", how="inner")
        .select("game_date", "team_id", "player_id", "player_name", "status")
    )
    inj = (
        inj.with_columns(
            severity=pl.col("status").replace_strict(_STATUS_SEVERITY, default=99)
        )
        .sort(["game_date", "team_id", "player_id", "severity"])
        .unique(subset=["game_date", "team_id", "player_id"], keep="first")
        .drop("severity")
    )
    if date_to_season is not None:
        inj = inj.join(date_to_season, on="game_date", how="inner")
    return inj


def _team_games(pg: pl.DataFrame) -> pl.DataFrame:
    """Every game each team played, from the box score."""
    return pg.select("season", "team_id", "game_id", "game_date").unique()


def membership_intervals(
    pg: pl.DataFrame, injuries: pl.DataFrame | None = None
) -> pl.DataFrame:
    """Per (season, team, player), the first and last date of dated evidence.

    A pair with no dated evidence -- on the season roster but never appearing
    and never reported -- is excluded: there is no way to tell when they joined,
    and a player who never played and was never reported carries no information
    about the hierarchy.
    """
    evidence = pg.select("season", "team_id", "player_id", "game_date").with_columns(
        source=pl.lit("box")
    )
    if injuries is not None and injuries.height:
        evidence = pl.concat(
            [
                evidence,
                injuries.select(
                    "season", "team_id", "player_id", "game_date"
                ).with_columns(source=pl.lit("injury")),
            ],
            how="vertical",
        )
    return evidence.group_by(["season", "team_id", "player_id"]).agg(
        pl.col("game_date").min().alias("tenure_start"),
        pl.col("game_date").max().alias("tenure_end"),
        (pl.col("source") == "box").sum().alias("n_box_rows"),
        (pl.col("source") == "injury").sum().alias("n_injury_rows"),
    )


def build_panel(
    data_dir: Path = DATA_DIR,
    season_types: tuple[str, ...] = SEASON_TYPES,
    pg: pl.DataFrame | None = None,
) -> pl.DataFrame:
    """The roster-complete panel, sorted for windowed feature computation."""
    if pg is None:
        pg = load_player_games(data_dir, season_types)

    date_to_season = pg.select("game_date", "season").unique()
    injuries = load_injury_reports(data_dir, date_to_season)
    # Keep only injury rows for a team the player has box score evidence with,
    # or the report's own team: a stale report naming a former team would
    # otherwise invent membership.
    intervals = membership_intervals(pg, injuries)
    team_games = _team_games(pg)

    panel = (
        intervals.join(team_games, on=["season", "team_id"], how="inner")
        .filter(
            (pl.col("game_date") >= pl.col("tenure_start"))
            & (pl.col("game_date") <= pl.col("tenure_end"))
        )
        .select("season", "team_id", "player_id", "game_id", "game_date")
    )

    # Attach what the box score knows, then what the injury report knows.
    panel = panel.join(
        pg.drop("season", "game_date"),
        on=["game_id", "team_id", "player_id"],
        how="left",
    ).join(
        injuries.select(
            "game_date", "team_id", "player_id",
            pl.col("status").alias("report_status"),
        ),
        on=["game_date", "team_id", "player_id"],
        how="left",
    )

    in_box = pl.col("status").is_not_null()
    panel = panel.with_columns(
        played=pl.col("played").fill_null(False),
        started=pl.col("started").fill_null(False),
        with_team=in_box,
        presence=pl.when(in_box)
        .then(pl.col("status"))
        .when(pl.col("report_status").is_in(_ABSENT_STATUSES))
        .then(pl.lit(PRESENCE_ABSENT_INJ))
        .otherwise(pl.lit(PRESENCE_ABSENT_UNKNOWN)),
    )

    # Identity columns come from the box score join, so they are null on rows
    # the player was absent for. Backfill them per player and per team.
    # Some players only ever appear in the injury report -- never in a box
    # score -- so it is a second source of names.
    names = (
        pl.concat(
            [
                pg.select("player_id", "player_name"),
                injuries.select("player_id", "player_name"),
            ],
            how="vertical",
        )
        .drop_nulls()
        .unique(subset=["player_id"], keep="last")
    )
    abbrs = pg.select("team_id", "team_abbreviation").drop_nulls().unique(
        subset=["team_id"], keep="last"
    )
    panel = (
        panel.join(names, on="player_id", how="left", suffix="_ref")
        .join(abbrs, on="team_id", how="left", suffix="_ref")
        .with_columns(
            player_name=pl.col("player_name").fill_null(pl.col("player_name_ref")),
            team_abbreviation=pl.col("team_abbreviation").fill_null(
                pl.col("team_abbreviation_ref")
            ),
        )
        .drop("player_name_ref", "team_abbreviation_ref")
    )

    panel = panel.sort(["player_id", "game_date", "game_id"])
    # Length of the consecutive absence run each row belongs to, so a model can
    # discount a run long enough to be a roster artefact rather than an injury.
    absent = ~pl.col("with_team")
    panel = panel.with_columns(
        _run=(absent != absent.shift(1).fill_null(False)).cum_sum().over("player_id")
    )
    panel = panel.with_columns(
        absence_run=pl.when(absent)
        .then(pl.len().over(["player_id", "_run"]))
        .otherwise(0)
    ).drop("_run")
    return panel

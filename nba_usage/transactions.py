"""Roster movement: who the team just gained or lost, outside of injuries.

A vacancy created by a trade is not an injury and does not appear on the
injury report, but it redistributes minutes the same way. A player who
arrived last week is also an unknown quantity to the rotation in a way their
own rolling history cannot express, because that history was accumulated
somewhere else.

The log is keyed by player name and team nickname rather than ids, so names
are resolved through the util crosswalk.
"""

from __future__ import annotations

import polars as pl

from .panel import DATA

CHURN_WINDOW_DAYS = 30


def _name_to_id() -> pl.DataFrame:
    return (
        pl.read_parquet(DATA / "util" / "active_player_vw.parquet")
        .filter(pl.col("platform") == "nba")
        .select(pl.col("source_name").alias("player"), pl.col("source_id").alias("player_id"))
        .unique(subset=["player"])
    )


def _team_nickname_map() -> pl.DataFrame:
    teams = pl.read_parquet(DATA / "nba" / "teams.parquet")
    slug = next(c for c in teams.columns if c.lower() in {"team_slug", "team_abbreviation"})
    nick = next(c for c in teams.columns if c.lower() == "team_name")
    return teams.select(pl.col(nick).alias("team"), pl.col(slug).alias("team_abbreviation")).unique()


def movements() -> pl.DataFrame:
    """Trades and signings only; injury rows duplicate the report we already use."""
    log = (
        pl.read_parquet(DATA / "nba" / "transaction_log.parquet")
        .filter(pl.col("transaction_type") == "Movement")
        .select("date", "team", "player", "acc_req")
    )
    return (
        log.join(_team_nickname_map(), on="team", how="inner")
        .join(_name_to_id(), on="player", how="left")
        .select("date", "team_abbreviation", "player_id", "acc_req")
    )


def add_transaction_features(frame: pl.DataFrame) -> pl.DataFrame:
    """Days since the player arrived, and how much the roster has churned."""
    mv = movements()

    arrivals = (
        mv.filter((pl.col("acc_req") == "Acquired") & pl.col("player_id").is_not_null())
        .select("player_id", "team_abbreviation", acquired_on="date")
        .sort("acquired_on")
    )
    out = (
        frame.sort("game_date")
        .join_asof(
            arrivals.sort("acquired_on"),
            left_on="game_date",
            right_on="acquired_on",
            by=["player_id", "team_abbreviation"],
            strategy="backward",
        )
        .with_columns(
            days_since_acquired=(pl.col("game_date") - pl.col("acquired_on")).dt.total_days()
        )
        .drop("acquired_on")
    )

    # Team churn: movements in and out over the last month, counted per team
    # per day and rolled forward.
    churn = (
        mv.group_by(["team_abbreviation", "date"])
        .agg(
            arrived=(pl.col("acc_req") == "Acquired").sum(),
            departed=(pl.col("acc_req") == "Relinquished").sum(),
        )
        .sort("date")
        .rolling(index_column="date", period=f"{CHURN_WINDOW_DAYS}d", closed="left", group_by="team_abbreviation")
        .agg(
            team_arrivals_30d=pl.col("arrived").sum(),
            team_departures_30d=pl.col("departed").sum(),
        )
    )

    return (
        out.sort("game_date")
        .join_asof(
            churn.sort("date"),
            left_on="game_date",
            right_on="date",
            by="team_abbreviation",
            strategy="backward",
        )
        .with_columns(
            days_since_acquired=pl.col("days_since_acquired").fill_null(9999).clip(0, 9999),
            team_arrivals_30d=pl.col("team_arrivals_30d").fill_null(0),
            team_departures_30d=pl.col("team_departures_30d").fill_null(0),
            recently_acquired=(pl.col("days_since_acquired") <= 30),
        )
        .drop("date")
    )


TRANSACTION_COLS = [
    "days_since_acquired",
    "recently_acquired",
    "team_arrivals_30d",
    "team_departures_30d",
]

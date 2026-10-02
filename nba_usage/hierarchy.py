"""Team hierarchy: where a player sits in the healthy rotation, and how big a
share of the available load that makes them.

The absence features in `injuries.py` say how much usage a team is missing.
They say nothing about who picks it up. These features give each player a
position in the night's pecking order, computed from prior games only:
being the second option behind a healthy star is a different job from being
the second option when the star is out.

Availability is taken from the active roster for the game minus anyone the
injury report rules out. Both are known before tip-off: the inactive list is
published pre-game, and so is the report.
"""

from __future__ import annotations

import polars as pl

from .injuries import load_injuries, player_state


def add_rotation_features(panel: pl.DataFrame, played: pl.DataFrame) -> pl.DataFrame:
    """Per player-game: rank and share within that night's healthy rotation."""
    state = player_state(played)

    # Each dressed player's role level as of their last prior game.
    rows = (
        panel.select("game_id", "game_date", "team_abbreviation", "player_id")
        .sort(["player_id", "game_date"])
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

    out_players = (
        load_injuries().filter(pl.col("is_out")).select("game_id", "player_id").unique()
    )
    rows = rows.join(out_players.with_columns(ruled_out=True), on=["game_id", "player_id"], how="left").with_columns(
        available=pl.col("ruled_out").is_null()
    )

    # Rank and shares are computed over the available players only, so a
    # player's standing rises automatically when those above them sit out.
    avail = pl.col("available")
    load = pl.col("state_usg") * pl.col("state_min")
    grp = ["game_id", "team_abbreviation"]

    rows = rows.with_columns(
        rotation_rank=pl.when(avail)
        .then(pl.col("state_min"))
        .otherwise(None)
        .rank("ordinal", descending=True)
        .over(grp),
        rotation_size=avail.sum().over(grp),
        team_avail_min=pl.when(avail).then(pl.col("state_min")).otherwise(0.0).sum().over(grp),
        team_avail_load=pl.when(avail).then(load).otherwise(0.0).sum().over(grp),
        team_avail_top_min=pl.when(avail).then(pl.col("state_min")).otherwise(None).max().over(grp),
    ).with_columns(
        expected_min_share=(pl.col("state_min") / pl.col("team_avail_min").replace(0.0, None)).fill_null(0.0),
        expected_load_share=(load / pl.col("team_avail_load").replace(0.0, None)).fill_null(0.0),
        rotation_rank_norm=(pl.col("rotation_rank") / pl.col("rotation_size").replace(0, None)).fill_null(1.0),
        is_top_option=(pl.col("rotation_rank") == 1),
        min_gap_to_top=(pl.col("team_avail_top_min") - pl.col("state_min")),
    )

    return rows.select(
        "game_id",
        "player_id",
        "team_abbreviation",
        *ROTATION_COLS,
    )


ROTATION_COLS = [
    "rotation_rank",
    "rotation_rank_norm",
    "rotation_size",
    "expected_min_share",
    "expected_load_share",
    "is_top_option",
    "min_gap_to_top",
    "team_avail_min",
]

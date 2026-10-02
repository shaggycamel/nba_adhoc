"""A dedicated minutes model's features.

Minutes is the factor injury news actually determines, and the one with the
most variance in the stepping-up case, so it gets features of its own rather
than reusing the usage set.

Two kinds are added. The first is structural: a team plays 240 minutes, so a
player's share of the healthy rotation's minutes implies an allocation
directly, and that allocation rises on its own when teammates are ruled out.
The second is behavioural: some players are the ones a coach actually leans
on when a starter sits, and that is measurable from prior seasons.
"""

from __future__ import annotations

import polars as pl

TEAM_GAME_MINUTES = 240.0
SHRINKAGE_K = 10.0


def add_allocation_features(frame: pl.DataFrame) -> pl.DataFrame:
    """Minutes implied by the player's share of the available rotation."""
    return frame.with_columns(
        expected_min_alloc=pl.col("expected_min_share") * TEAM_GAME_MINUTES,
    ).with_columns(
        # How much more than usual this allocation implies. On a full roster
        # this sits near zero; it grows as the rotation thins.
        min_uplift=pl.col("expected_min_alloc") - pl.col("min_r10"),
        # The vacated minutes this player should absorb on share alone.
        min_absorb_by_share=pl.col("expected_min_share") * pl.col("vacated_min"),
    )


def add_responsiveness(frame: pl.DataFrame) -> pl.DataFrame:
    """How much each player's minutes actually rise when a starter sits.

    Estimated per season from earlier seasons only, and shrunk towards zero
    by how few short-handed games the player has, so a rookie with two such
    games does not get a large coefficient.
    """
    seasons = sorted(frame["season"].unique().to_list())
    short_handed = pl.col("vacated_starter_min") >= 1

    scored = []
    for i, season in enumerate(seasons):
        cur = frame.filter(pl.col("season") == season).select("game_id", "player_id")
        if i == 0:
            scored.append(cur.with_columns(min_responsiveness=pl.lit(None, dtype=pl.Float64)))
            continue
        prior = frame.filter(pl.col("season").is_in(seasons[:i]))
        eff = (
            prior.group_by("player_id")
            .agg(
                m_short=pl.col("min").filter(short_handed).mean(),
                m_full=pl.col("min").filter(~short_handed).mean(),
                n_short=short_handed.sum(),
            )
            .drop_nulls(["m_short", "m_full"])
            .with_columns(
                min_responsiveness=(pl.col("m_short") - pl.col("m_full"))
                * (pl.col("n_short") / (pl.col("n_short") + SHRINKAGE_K))
            )
            .select("player_id", "min_responsiveness")
        )
        scored.append(cur.join(eff, on="player_id", how="left"))

    return frame.join(pl.concat(scored), on=["game_id", "player_id"], how="left").with_columns(
        min_responsiveness=pl.col("min_responsiveness").fill_null(0.0)
    )


MINUTES_COLS = ["expected_min_alloc", "min_uplift", "min_absorb_by_share", "min_responsiveness"]

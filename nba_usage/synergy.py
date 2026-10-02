"""Pairwise with/without effects: whose absence actually lifts this player.

`vacated_usg` treats every absent teammate as interchangeable. In practice a
guard's usage jumps when the ball-dominant guard sits and barely moves when
the backup centre does. This estimates, for each ordered pair (player,
teammate), how much the player's usage shifts in games the teammate missed,
shrunk towards zero by how little evidence there is.

Effects are refitted for every season using only earlier seasons, so a row's
feature never depends on its own game or on anything later.
"""

from __future__ import annotations

import polars as pl

# Games-without needed before a pair's raw difference is believed at all.
# A pair seen twice carries almost no weight; one seen 40 times carries most.
SHRINKAGE_K = 20.0
MIN_PAIR_GAMES = 5


def _pairs(panel: pl.DataFrame) -> pl.DataFrame:
    """Every (focal, teammate) pair that was on the same roster for a game.

    The focal row must be a game the player actually played, since usage is
    only observed then. The teammate contributes whether they played or not.
    """
    focal = panel.filter(pl.col("played")).select(
        "game_id", "season", "team_abbreviation", "player_id", "usg_pct"
    )
    mate = panel.select(
        "game_id",
        "team_abbreviation",
        pl.col("player_id").alias("teammate_id"),
        pl.col("played").alias("mate_played"),
    )
    return focal.join(mate, on=["game_id", "team_abbreviation"], how="inner").filter(
        pl.col("player_id") != pl.col("teammate_id")
    )


def fit_pair_effects(pairs: pl.DataFrame) -> pl.DataFrame:
    """Shrunk usage difference for each pair: with the teammate out, minus with them in."""
    agg = pairs.group_by(["player_id", "teammate_id"]).agg(
        usg_with=pl.col("usg_pct").filter(pl.col("mate_played")).mean(),
        usg_without=pl.col("usg_pct").filter(~pl.col("mate_played")).mean(),
        n_with=pl.col("mate_played").sum(),
        n_without=(~pl.col("mate_played")).sum(),
    )
    return (
        agg.filter((pl.col("n_without") >= MIN_PAIR_GAMES) & (pl.col("n_with") >= MIN_PAIR_GAMES))
        .with_columns(raw_delta=pl.col("usg_without") - pl.col("usg_with"))
        .with_columns(
            # Shrink towards zero: a pair with few games-without is mostly noise.
            pair_delta=pl.col("raw_delta")
            * (pl.col("n_without") / (pl.col("n_without") + SHRINKAGE_K))
        )
        .select("player_id", "teammate_id", "pair_delta", "n_without")
    )


def absorption_features(panel: pl.DataFrame) -> pl.DataFrame:
    """Per player-game: expected usage gain from exactly who is missing tonight.

    Walks the seasons in order, fitting pair effects on everything earlier and
    scoring only the current season with them, so no row is scored by an
    effect its own game helped estimate.
    """
    pairs = _pairs(panel)
    seasons = sorted(panel["season"].unique().to_list())

    scored = []
    for i, season in enumerate(seasons):
        if i == 0:
            continue
        effects = fit_pair_effects(pairs.filter(pl.col("season").is_in(seasons[:i])))
        if effects.is_empty():
            continue
        this = pairs.filter(pl.col("season") == season).filter(~pl.col("mate_played"))
        scored.append(
            this.join(effects, on=["player_id", "teammate_id"], how="inner")
            .group_by(["game_id", "player_id"])
            .agg(
                absorb_delta=pl.col("pair_delta").sum(),
                absorb_delta_max=pl.col("pair_delta").max(),
                n_absent_known=pl.len(),
            )
        )

    if not scored:
        return panel.select("game_id", "player_id").with_columns(
            absorb_delta=pl.lit(0.0), absorb_delta_max=pl.lit(0.0), n_absent_known=pl.lit(0)
        )

    allscored = pl.concat(scored)
    return (
        panel.select("game_id", "player_id")
        .join(allscored, on=["game_id", "player_id"], how="left")
        .with_columns(
            absorb_delta=pl.col("absorb_delta").fill_null(0.0),
            absorb_delta_max=pl.col("absorb_delta_max").fill_null(0.0),
            n_absent_known=pl.col("n_absent_known").fill_null(0),
        )
    )


ABSORPTION_COLS = ["absorb_delta", "absorb_delta_max", "n_absent_known"]

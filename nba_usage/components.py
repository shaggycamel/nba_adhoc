"""Does predicting usage's components beat predicting usage directly?

The NBA's usg_pct is a share, and the identity rearranges to something
simpler than it first looks:

    usg = (FGA + 0.44*FTA + TOV) * (team_min/5) / (min * team_events)
        = (player events per minute) / (team events per game-minute)

So the player's own minutes cancel: usage is a ratio of two rates. That gives
three ways to attack it, all scored on the same folds with the same features,
so the only thing varying is how the target is parameterised.

    direct      one model on usg_pct                        (the current best)
    rate ratio  one model on the player's event rate,
                one on the team's, then divide
    components  separate models for FGA, FTA and TOV per
                minute, recombined, then divided by the team rate

Note the assist term in the Hollinger-style formula is absent: the NBA stat
this project targets does not use assists, which `verify_identity` confirms
against the stored column.
"""

from __future__ import annotations

import numpy as np
import polars as pl

from .evaluate import metrics
from .panel import DATA

# The weight the box-score identity gives free throws: ~0.44 of a possession.
FT_WEIGHT = 0.44


def component_targets() -> pl.DataFrame:
    """Player event rate, team event rate, and the usage they reconstruct."""
    box = pl.read_parquet(DATA / "nba" / "player_box_score.parquet").select(
        "game_id", "team_id", "player_id", "min", "fga", "fta", "tov", "usg_pct"
    )
    team = box.group_by(["game_id", "team_id"]).agg(
        t_fga=pl.col("fga").sum(),
        t_fta=pl.col("fta").sum(),
        t_tov=pl.col("tov").sum(),
        t_min=pl.col("min").sum(),
    )
    out = (
        box.join(team, on=["game_id", "team_id"])
        .filter((pl.col("min") > 0) & pl.col("usg_pct").is_not_null())
        .with_columns(
            fga_pm=pl.col("fga") / pl.col("min"),
            fta_pm=pl.col("fta") / pl.col("min"),
            tov_pm=pl.col("tov") / pl.col("min"),
            team_rate=(pl.col("t_fga") + FT_WEIGHT * pl.col("t_fta") + pl.col("t_tov"))
            / (pl.col("t_min") / 5),
        )
        .with_columns(
            player_rate=pl.col("fga_pm") + FT_WEIGHT * pl.col("fta_pm") + pl.col("tov_pm")
        )
        .with_columns(usg_recon=pl.col("player_rate") / pl.col("team_rate"))
    )
    return out.select(
        "game_id", "player_id", "fga_pm", "fta_pm", "tov_pm", "player_rate", "team_rate", "usg_recon"
    )


def verify_identity(frame: pl.DataFrame) -> dict[str, float]:
    """How close the reconstruction gets before any model is involved.

    This is the floor any component approach inherits: minutes are stored as
    whole numbers in this dataset and usg_pct to three decimals, so the
    identity cannot be reproduced exactly.
    """
    err = (frame["usg_recon"] - frame["usg_pct"]).abs()
    return {
        "mae": float(err.mean()),
        "median": float(err.median()),
        "p90": float(err.quantile(0.9)),
        "corr": float(frame.select(pl.corr("usg_recon", "usg_pct")).item()),
    }


def _fit_predict(fit, train: pl.DataFrame, valid: pl.DataFrame, cols: list[str], target: str) -> np.ndarray:
    """Fit one model on a given target and return its validation predictions."""
    tr = train.drop_nulls(target)
    m, model = fit(tr, valid, cols, target=target)
    from .models import design_matrix

    pred = model.predict(design_matrix(valid, cols))
    return np.asarray(pred)


def compose_rate_ratio(fit, train, valid, cols) -> dict[str, float]:
    """Predict the player's event rate and the team's, then divide."""
    p = _fit_predict(fit, train, valid, cols, "player_rate")
    t = _fit_predict(fit, train, valid, cols, "team_rate")
    # A predicted team rate near zero would blow up the ratio; clip to the
    # range the training seasons actually contain.
    lo, hi = train["team_rate"].quantile(0.001), train["team_rate"].quantile(0.999)
    pred = p / np.clip(t, lo, hi)
    return metrics(valid["usg_pct"].to_numpy(), pred)


def compose_components(fit, train, valid, cols) -> dict[str, float]:
    """Predict FGA, FTA and TOV per minute separately, then recombine."""
    fga = _fit_predict(fit, train, valid, cols, "fga_pm")
    fta = _fit_predict(fit, train, valid, cols, "fta_pm")
    tov = _fit_predict(fit, train, valid, cols, "tov_pm")
    t = _fit_predict(fit, train, valid, cols, "team_rate")
    lo, hi = train["team_rate"].quantile(0.001), train["team_rate"].quantile(0.999)
    pred = (fga + FT_WEIGHT * fta + tov) / np.clip(t, lo, hi)
    return metrics(valid["usg_pct"].to_numpy(), pred)

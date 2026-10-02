"""Project box-score volume (points, rebounds, assists) from the usage model.

The factorisation is the one the usage identity implies, read forwards:

    stat  =  minutes  x  (stat per minute)

Minutes is what moves when a starter sits, so it gets its own model. The
per-minute rate gets the usage prediction as an input, because a player's
share of the offence is most of what decides their scoring rate and some of
their assist rate. Rebounds are included as a control: usage should do very
little for them, and if it appears to help there, something is leaking.

Usage predictions fed to the rate models are always out-of-fold. A usage
model trained on the same rows the rate model trains on has seen those rows'
targets, and the rate model would over-trust it: good validation numbers,
disappointing live ones.
"""

from __future__ import annotations

import numpy as np
import polars as pl

from .evaluate import metrics
from .features import ROLE_STATS, add_prior_features
from .models import design_matrix

VOLUME_STATS = ["pts", "reb", "ast"]
# History for the volume targets as well as the usage-role stats.
VOLUME_ROLE_STATS = ROLE_STATS + VOLUME_STATS


def add_volume_features(played: pl.DataFrame, **kwargs) -> pl.DataFrame:
    """Prior-game history including scoring, rebounding and assist rates."""
    return add_prior_features(played, stats=VOLUME_ROLE_STATS, **kwargs)


def add_volume_targets(frame: pl.DataFrame) -> pl.DataFrame:
    """Per-minute versions of each volume stat, alongside the raw totals."""
    return frame.with_columns(
        [(pl.col(s) / pl.col("min")).alias(f"{s}_pm") for s in VOLUME_STATS]
    )


def oof_usage(
    frame: pl.DataFrame,
    cols: list[str],
    fit,
    target: str = "usg_pct",
    out_col: str = "usg_hat",
) -> pl.DataFrame:
    """A usage prediction for every row, never fitted on that row's season.

    Walks seasons in order and predicts each from a model trained on the
    earlier ones, which is the same discipline the rest of the project uses
    and makes the column safe to feed downstream.
    """
    seasons = sorted(frame["season"].unique().to_list())
    preds = []
    for i, season in enumerate(seasons):
        cur = frame.filter(pl.col("season") == season)
        if i == 0:
            preds.append(cur.select("game_id", "player_id").with_columns(
                pl.lit(None, dtype=pl.Float64).alias(out_col)
            ))
            continue
        prior = frame.filter(pl.col("season").is_in(seasons[:i]))
        _, model = fit(prior, cur, cols, target=target)
        p = np.asarray(model.predict(design_matrix(cur, cols)))
        preds.append(cur.select("game_id", "player_id").with_columns(pl.Series(out_col, p)))
    return frame.join(pl.concat(preds), on=["game_id", "player_id"], how="left")


def fit_minutes(fit, train: pl.DataFrame, valid: pl.DataFrame, cols: list[str]) -> np.ndarray:
    """Predicted minutes, floored at zero and capped at the longest game seen."""
    _, model = fit(train, valid, cols, target="min")
    pred = np.asarray(model.predict(design_matrix(valid, cols)))
    return np.clip(pred, 0.0, float(train["min"].max()))


def compose_volume(
    fit,
    train: pl.DataFrame,
    valid: pl.DataFrame,
    cols: list[str],
    stat: str,
    minutes_pred: np.ndarray | None = None,
) -> tuple[dict[str, float], np.ndarray]:
    """minutes x per-minute rate, each from its own model."""
    mins = fit_minutes(fit, train, valid, cols) if minutes_pred is None else minutes_pred
    _, rate_model = fit(train.drop_nulls(f"{stat}_pm"), valid, cols, target=f"{stat}_pm")
    rate = np.asarray(rate_model.predict(design_matrix(valid, cols)))
    pred = np.clip(mins * np.clip(rate, 0.0, None), 0.0, None)
    return metrics(valid[stat].to_numpy(), pred), pred


def fit_direct_volume(fit, train: pl.DataFrame, valid: pl.DataFrame, cols: list[str], stat: str) -> dict[str, float]:
    """One model straight onto the stat total, for comparison."""
    _, model = fit(train, valid, cols, target=stat)
    pred = np.clip(np.asarray(model.predict(design_matrix(valid, cols))), 0.0, None)
    return metrics(valid[stat].to_numpy(), pred)


def volume_baselines(train: pl.DataFrame, valid: pl.DataFrame, stat: str) -> dict[str, dict[str, float]]:
    """What a model has to beat: the player's recent average of the stat."""
    y = valid[stat].to_numpy()
    out = {}
    for name, col in (("last5_mean", f"{stat}_r5"), ("last10_mean", f"{stat}_r10"), ("season_mean", f"{stat}_season")):
        if col not in valid.columns:
            continue
        fallback = float(train[stat].mean())
        pred = valid[col].fill_null(fallback).fill_nan(fallback).to_numpy()
        out[name] = metrics(y, pred)
    return out


def volume_feature_columns(df: pl.DataFrame, extra: list[str] | None = None) -> list[str]:
    """Lagged history for the volume stats as well as the usage-role stats."""
    from .features import _GENERATED

    engineered = [
        c
        for c in df.columns
        if any(c.startswith(f"{s}_") for s in VOLUME_ROLE_STATS) and _GENERATED.search(c)
    ]
    context = [
        "home", "days_rest_capped", "is_b2b", "season_game_n", "career_game_n",
        "usg_trend_short", "usg_vs_season",
    ]
    cols = [c for c in engineered + context + (extra or []) if c in df.columns]
    return list(dict.fromkeys(cols))

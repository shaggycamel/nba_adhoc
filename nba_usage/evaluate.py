"""Time-based splits, metrics and the baselines every model must beat."""

from __future__ import annotations

from dataclasses import dataclass

import numpy as np
import polars as pl


@dataclass(frozen=True)
class Split:
    """A contiguous block of seasons for train / validation / test."""

    name: str
    train: tuple[str, ...]
    valid: tuple[str, ...]


def season_splits(seasons: list[str], n_folds: int = 3, min_train: int = 3) -> list[Split]:
    """Expanding-window folds over seasons: train on the past, score the next.

    Never shuffles. Fold k trains on everything up to a season and validates
    on the single season that follows it.
    """
    seasons = sorted(seasons)
    folds = []
    for i in range(len(seasons) - n_folds, len(seasons)):
        if i < min_train:
            continue
        folds.append(Split(name=seasons[i], train=tuple(seasons[:i]), valid=(seasons[i],)))
    return folds


def metrics(y_true: np.ndarray, y_pred: np.ndarray) -> dict[str, float]:
    err = y_pred - y_true
    ss_res = float(np.sum(err**2))
    ss_tot = float(np.sum((y_true - y_true.mean()) ** 2))
    return {
        "mae": float(np.mean(np.abs(err))),
        "rmse": float(np.sqrt(np.mean(err**2))),
        "r2": 1.0 - ss_res / ss_tot if ss_tot > 0 else float("nan"),
        "n": int(len(y_true)),
    }


# --- baselines -------------------------------------------------------------
# Each takes the validation frame and returns a prediction built only from
# columns that are already lagged, so none of them peek at the current game.

BASELINES: dict[str, str] = {
    "global_mean": "",           # handled specially: train-set mean
    "career_mean": "usg_pct_career",
    "season_mean": "usg_pct_season",
    "last3_mean": "usg_pct_r3",
    "last5_mean": "usg_pct_r5",
    "last10_mean": "usg_pct_r10",
    "ewm3": "usg_pct_ewm3",
}


def run_baselines(train: pl.DataFrame, valid: pl.DataFrame, target: str = "usg_pct") -> pl.DataFrame:
    """Score every baseline on one fold.

    A baseline with no history for a row (a debut, say) falls back to the
    training mean rather than being dropped, so all baselines are scored on an
    identical row set and the comparison stays fair.
    """
    train_mean = float(train[target].mean())
    y = valid[target].to_numpy()
    rows = []
    for name, col in BASELINES.items():
        if name == "global_mean":
            pred = np.full(len(y), train_mean)
        else:
            pred = valid[col].fill_null(train_mean).fill_nan(train_mean).to_numpy()
        rows.append({"model": name, **metrics(y, pred)})
    return pl.DataFrame(rows)

"""Layer 3: will this player take the floor?

The injury report is the only pre-tip-off source that names who is expected to
miss a game, and it is the layer the whole hierarchy leans on: a depth chart
that ignores availability ranks injured players above healthy ones.

Three things about the source shape this module.

It starts on 2021-10-19, so this layer is restricted to that era while layers 1
and 2 train on 2009-10 onward.

It is a forecast, not an outcome. 175 players listed Out went on to play. So the
status is a feature, never a label, and the target is what actually happened
(`played`), which is what makes the output a calibrated probability rather than
a restatement of the report.

Its duplicates are exact repeats rather than successive revisions -- of 62,840
(date, player, team) groups only 478 repeat, and in each the status, reason and
game_id agree -- so there is no "last report before tip-off" to reconstruct.
That assumption is checked in `roster.load_injury_reports`.

`NO_REPORT` is a real and common state, not a missing value: most players on
most nights are simply not mentioned, which is itself strong evidence they are
available.

Measured findings, walk-forward on 2023-24 to 2025-26 with each season trained
only on seasons that finished before it:

    model                   log loss   Brier     AUC     ECE
    base rate                 0.6708  0.2389  0.5000  0.0111
    status only               0.4144  0.1336  0.8030  0.0549
    state only                0.3785  0.1171  0.9019  0.0105
    state + report            0.2548  0.0799  0.9556  0.0205
    state + report + isotonic 0.2561  0.0805  0.9544  0.0118

The report is worth a third of the log loss over knowing only who has been
playing lately, so injury earns its place rather than being assumed into the
model. The less comfortable half of that result is that the report alone is
*worse* than the recent record alone: it is a strong complement, not a
substitute.

Two cautions for anyone reading per-status rates directly:

* Questionable is a genuine coin flip -- 47.6% of those players play -- so it
  must stay a probability and never be collapsed to a yes or no.
* 2021-22 is not comparable to later seasons. The report covered only 4.9% of
  player-games against ~30% since, and an "Out" that season still played 8.2%
  of the time versus 0.1-0.6% later. Excluding it from training was tested and
  changed nothing (log loss 0.2586 against 0.2565), so it is kept for the extra
  data, but any statistic cut by status should exclude it.
"""

from __future__ import annotations

import numpy as np
import polars as pl

from .config import INJURY_ERA_START

NO_REPORT = "NO_REPORT"

# Report statuses from most to least doubtful, plus the absence of a report.
REPORT_LEVELS = ("Out", "Doubtful", "Questionable", "Probable", "Available", NO_REPORT)

# What the report says, now and recently.
REPORT_FEATURES = (
    "report_level",
    "report_out_streak",
    "prior_report_level",
    "report_is_new",
)

# What the player's recent record says, independent of any report. This is the
# ablation that matters: the report has to beat this to be worth anything.
STATE_FEATURES = (
    "play_rate_5",
    "play_rate_10",
    "play_rate_20",
    "with_team_rate_5",
    "with_team_rate_10",
    "coach_dnp_rate_10",
    "days_since_played",
    "days_rest",
    "team_games_missed",
    "absent_streak_prior",
    "ewm_min_8",
    "ewm_min_20",
    "ewm_min_played_8",
    "depth_rank",
    "team_size",
    "career_games_prior",
    "season_games_prior",
)

TARGET = "played"


def add_report_features(panel: pl.DataFrame) -> pl.DataFrame:
    """Encode the current report plus a short history of it.

    `report_status` is attached by `roster.build_panel` and is known before
    tip-off, so using it for the current game is legitimate rather than leakage.
    The history columns are shifted and look only backwards.
    """
    levels = {s: i for i, s in enumerate(REPORT_LEVELS)}
    out = panel.sort(["player_id", "game_date", "game_id"]).with_columns(
        _status=pl.col("report_status").fill_null(NO_REPORT)
    )
    out = out.with_columns(
        report_level=pl.col("_status").replace_strict(levels).cast(pl.Int32)
    )
    prior = pl.col("report_level").shift(1).over("player_id")
    is_out = pl.col("_status") == "Out"
    # Consecutive prior games carrying an Out report, counted backwards only.
    idx = pl.int_range(pl.len(), dtype=pl.Int32).over("player_id")
    not_out_idx = pl.when(~is_out).then(idx).otherwise(None)
    # Index of the most recent prior game NOT carrying an Out report. Filling
    # with -1 covers the player whose every prior game was an Out: there is no
    # such game, and the streak is then simply his number of prior games.
    last_not_out = not_out_idx.shift(1).forward_fill().over("player_id").fill_null(-1)
    return out.with_columns(
        prior_report_level=prior,
        report_out_streak=(idx - 1 - last_not_out).clip(0),
        # A first appearance has no previous status to differ from, which counts
        # as new rather than unknown.
        report_is_new=(pl.col("report_level") != prior)
        .fill_null(True)
        .cast(pl.Int32),
    ).drop("_status")


def injury_era(df: pl.DataFrame) -> pl.DataFrame:
    return df.filter(pl.col("game_date") >= INJURY_ERA_START)


def _matrix(df: pl.DataFrame, features: tuple[str, ...]) -> np.ndarray:
    return df.select(features).to_numpy().astype(np.float64)


def fit_play_model(train: pl.DataFrame, features: tuple[str, ...], **kwargs):
    """Binary classifier for whether a player plays, trained on log loss."""
    import lightgbm as lgb

    params = {
        "objective": "binary",
        "learning_rate": 0.05,
        "num_leaves": 31,
        "min_child_samples": 100,
        "n_estimators": 400,
        "verbose": -1,
        "random_state": 0,
    }
    params.update(kwargs)
    model = lgb.LGBMClassifier(**params)
    model.fit(_matrix(train, features), train[TARGET].to_numpy().astype(int))
    return model


def predict_play_probability(
    model, df: pl.DataFrame, features: tuple[str, ...], name: str = "p_play"
) -> pl.DataFrame:
    proba = model.predict_proba(_matrix(df, features))[:, 1]
    return df.with_columns(pl.Series(name, proba))


def status_rate_table(train: pl.DataFrame) -> dict[int, float]:
    """Empirical play rate per report level -- the status-only baseline."""
    agg = train.group_by("report_level").agg(pl.col(TARGET).mean().alias("rate"))
    return {int(r["report_level"]): float(r["rate"]) for r in agg.to_dicts()}


# ------------------------------------------------------------------ scoring


def log_loss(p: np.ndarray, y: np.ndarray, eps: float = 1e-15) -> float:
    p = np.clip(p, eps, 1 - eps)
    return float(-np.mean(y * np.log(p) + (1 - y) * np.log(1 - p)))


def brier(p: np.ndarray, y: np.ndarray) -> float:
    return float(np.mean((p - y) ** 2))


def auc(p: np.ndarray, y: np.ndarray) -> float:
    """Rank-based AUC, tie-corrected."""
    if y.min() == y.max():
        return float("nan")
    order = np.argsort(p, kind="mergesort")
    ranks = np.empty(len(p), dtype=np.float64)
    sorted_p = p[order]
    i = 0
    while i < len(p):
        j = i
        while j + 1 < len(p) and sorted_p[j + 1] == sorted_p[i]:
            j += 1
        ranks[order[i : j + 1]] = 0.5 * (i + j) + 1
        i = j + 1
    n_pos = float(y.sum())
    n_neg = float(len(y) - n_pos)
    return float((ranks[y == 1].sum() - n_pos * (n_pos + 1) / 2) / (n_pos * n_neg))


def expected_calibration_error(
    p: np.ndarray, y: np.ndarray, bins: int = 20
) -> float:
    """Mean gap between predicted and observed rates, weighted by bin size.

    A model can rank well and still be miscalibrated, and the hierarchy needs
    the probability itself -- it feeds expected minutes downstream.
    """
    edges = np.linspace(0.0, 1.0, bins + 1)
    idx = np.clip(np.digitize(p, edges[1:-1]), 0, bins - 1)
    total = 0.0
    for b in range(bins):
        mask = idx == b
        n = int(mask.sum())
        if n:
            total += n * abs(p[mask].mean() - y[mask].mean())
    return float(total / len(p))


def score(p: np.ndarray, y: np.ndarray) -> dict[str, float]:
    return {
        "log_loss": log_loss(p, y),
        "brier": brier(p, y),
        "auc": auc(p, y),
        "ece": expected_calibration_error(p, y),
    }


class CalibratedPlayModel:
    """A play model with an isotonic correction fitted on held-out later games.

    The raw gradient-boosted model discriminates well but comes out
    systematically under-confident in the middle of its range -- predicting
    0.45 where the observed rate is 0.50. Ranking is unaffected, but the
    hierarchy consumes the probability itself to form expected minutes, so the
    level matters as much as the order.

    The correction is fitted on the most recent slice of the training data
    rather than a random sample, so the calibration set sits in the same place
    relative to the test season that it will at serving time.
    """

    def __init__(self, model, calibrator, features: tuple[str, ...]):
        self.model = model
        self.calibrator = calibrator
        self.features = features

    def predict(self, df: pl.DataFrame) -> np.ndarray:
        raw = self.model.predict_proba(_matrix(df, self.features))[:, 1]
        return self.calibrator.predict(raw)


def fit_calibrated_play_model(
    train: pl.DataFrame,
    features: tuple[str, ...],
    calibration_fraction: float = 0.2,
    **kwargs,
) -> CalibratedPlayModel:
    """Fit the play model, holding back the latest games to calibrate on."""
    from sklearn.isotonic import IsotonicRegression

    ordered = train.sort(["game_date", "game_id"])
    split = int(ordered.height * (1 - calibration_fraction))
    fit_part, calib_part = ordered.head(split), ordered.tail(ordered.height - split)

    model = fit_play_model(fit_part, features, **kwargs)
    raw = model.predict_proba(_matrix(calib_part, features))[:, 1]
    calibrator = IsotonicRegression(out_of_bounds="clip", y_min=0.0, y_max=1.0)
    calibrator.fit(raw, calib_part[TARGET].to_numpy().astype(int))
    return CalibratedPlayModel(model, calibrator, features)

"""Duration models, from flat baselines up to gradient-boosted hazards.

All of them answer the same question — given what is known the moment a
player is ruled out, how many games will they miss — so they can be scored
against each other on the same spells with the same metrics.

Three families:

1. Baselines that ignore the features. A Kaplan-Meier mean for everybody,
   then one per injury type, then the player's own history. These matter
   because the duration distribution is so skewed (median 1 game, p99 30)
   that "predict the median" is a genuinely hard baseline to beat on MAE.
2. Discrete-time hazard models, which is where the real work happens: a
   regularised logistic regression, a random forest, sklearn's histogram
   gradient booster, and LightGBM, all fitted to the per-game "do they play
   the next game" target.
3. A direct regression on observed duration, fitted only to spells whose
   return was seen. Not a serious candidate — it is here to measure what
   discarding censored spells costs.
"""

from __future__ import annotations

from dataclasses import dataclass, field

import numpy as np
import polars as pl
from sklearn.compose import ColumnTransformer
from sklearn.ensemble import (
    HistGradientBoostingClassifier,
    RandomForestClassifier,
)
from sklearn.impute import SimpleImputer
from sklearn.linear_model import LogisticRegression
from sklearn.pipeline import Pipeline
from sklearn.preprocessing import OneHotEncoder, StandardScaler

from . import hazard as hz

MAX_K = 100


# --------------------------------------------------------------------------
# Baselines
# --------------------------------------------------------------------------

@dataclass
class KMBaseline:
    """Kaplan-Meier mean, optionally stratified, with shrinkage to the pool.

    Stratum estimates are shrunk toward the global mean by
    `n / (n + prior_weight)`, so a body-part/ailment cell seen twice does not
    get to assert its own mean.
    """

    by: tuple[str, ...] = ()
    prior_weight: float = 10.0
    name: str = "km"
    _global: float = 0.0
    _table: dict = field(default_factory=dict)

    def fit(self, spells: pl.DataFrame) -> "KMBaseline":
        d = spells["games_missed"].to_numpy()
        e = spells["event"].to_numpy()
        self._global = hz.km_mean(d, e)
        self._table = {}
        if self.by:
            for key, grp in spells.group_by(list(self.by)):
                dd = grp["games_missed"].to_numpy()
                ee = grp["event"].to_numpy()
                n = len(dd)
                raw = hz.km_mean(dd, ee)
                w = n / (n + self.prior_weight)
                self._table[tuple(key)] = w * raw + (1 - w) * self._global
        return self

    def predict(self, spells: pl.DataFrame) -> np.ndarray:
        if not self.by:
            return np.full(spells.height, self._global)
        keys = list(zip(*[spells[c].to_list() for c in self.by]))
        return np.array([self._table.get(k, self._global) for k in keys])


@dataclass
class PlayerHistoryBaseline:
    """The player's own mean prior duration, shrunk toward the global mean."""

    prior_weight: float = 3.0
    name: str = "player_history"
    _global: float = 0.0

    def fit(self, spells: pl.DataFrame) -> "PlayerHistoryBaseline":
        self._global = hz.km_mean(
            spells["games_missed"].to_numpy(), spells["event"].to_numpy()
        )
        return self

    def predict(self, spells: pl.DataFrame) -> np.ndarray:
        n = spells["prior_spells_career"].fill_null(0).to_numpy().astype(float)
        total = spells["prior_games_missed_career"].fill_null(0).to_numpy().astype(float)
        mean_prior = np.where(n > 0, total / np.maximum(n, 1), self._global)
        w = n / (n + self.prior_weight)
        return w * mean_prior + (1 - w) * self._global


# --------------------------------------------------------------------------
# Hazard models
# --------------------------------------------------------------------------

def _sklearn_pipeline(estimator, numeric: list[str], categorical: list[str], scale: bool):
    num_steps = [("impute", SimpleImputer(strategy="median"))]
    if scale:
        num_steps.append(("scale", StandardScaler()))
    pre = ColumnTransformer(
        [
            ("num", Pipeline(num_steps), numeric),
            (
                "cat",
                Pipeline(
                    [
                        ("impute", SimpleImputer(strategy="constant", fill_value="missing")),
                        ("oh", OneHotEncoder(handle_unknown="ignore", min_frequency=20)),
                    ]
                ),
                categorical,
            ),
        ],
        remainder="drop",
    )
    return Pipeline([("pre", pre), ("est", estimator)])


@dataclass
class HazardModel:
    """A binary classifier over per-missed-game rows, read as a hazard."""

    name: str
    kind: str
    numeric: list[str]
    categorical: list[str]
    params: dict = field(default_factory=dict)
    _model: object | None = None

    def _build(self):
        if self.kind == "logistic":
            return _sklearn_pipeline(
                LogisticRegression(max_iter=2000, **self.params),
                self.numeric, self.categorical, scale=True,
            )
        if self.kind == "forest":
            return _sklearn_pipeline(
                RandomForestClassifier(random_state=0, n_jobs=-1, **self.params),
                self.numeric, self.categorical, scale=False,
            )
        if self.kind == "histgb":
            return _sklearn_pipeline(
                HistGradientBoostingClassifier(random_state=0, **self.params),
                self.numeric, self.categorical, scale=False,
            )
        if self.kind == "lightgbm":
            import lightgbm as lgb

            return lgb.LGBMClassifier(random_state=0, verbose=-1, n_jobs=-1, **self.params)
        raise ValueError(f"unknown kind {self.kind!r}")

    def _frame(self, rows: pl.DataFrame):
        cols = self.numeric + self.categorical
        df = rows.select(cols).to_pandas()
        if self.kind == "lightgbm":
            for c in self.categorical:
                df[c] = df[c].astype("category")
        for c in self.numeric:
            if df[c].dtype == bool:
                df[c] = df[c].astype(float)
        return df

    def fit(self, rows: pl.DataFrame, target: str = "returns_next") -> "HazardModel":
        self._model = self._build()
        X = self._frame(rows)
        y = rows[target].to_numpy()
        if self.kind == "lightgbm":
            self._model.fit(X, y, categorical_feature=self.categorical)
        else:
            self._model.fit(X, y)
        return self

    def hazard(self, rows: pl.DataFrame) -> np.ndarray:
        if self._model is None:
            raise RuntimeError("fit first")
        return self._model.predict_proba(self._frame(rows))[:, 1]


def predict_from_grid(
    model: HazardModel, grid: pl.DataFrame, max_k: int = MAX_K
) -> pl.DataFrame:
    """Turn per-(spell, k) hazards into per-spell duration summaries."""
    h = model.hazard(grid)
    g = grid.select("spell_id", "k").with_columns(pl.Series("h", h))
    g = g.sort("spell_id", "k").with_columns(
        (1.0 - pl.col("h").clip(1e-9, 1 - 1e-9)).log().cum_sum().over("spell_id").exp()
        .alias("surv")
    )
    out = g.group_by("spell_id").agg(
        (1.0 + pl.col("surv").sum()).alias("pred_mean_games"),
        pl.col("k").filter(pl.col("surv") <= 0.5).min().alias("pred_median_games"),
        (1.0 - pl.col("surv").filter(pl.col("k") == 1).first()).alias("p_back_next_game"),
        (1.0 - pl.col("surv").filter(pl.col("k") == 3).first()).alias("p_back_within_3"),
        (1.0 - pl.col("surv").filter(pl.col("k") == 10).first()).alias("p_back_within_10"),
        pl.col("surv").filter(pl.col("k") == 20).first().alias("p_out_past_20"),
    )
    return out.with_columns(
        pl.col("pred_median_games").fill_null(max_k + 1).cast(pl.Float64)
    )


# --------------------------------------------------------------------------
# Direct regression on observed duration (censoring-blind, for contrast)
# --------------------------------------------------------------------------

@dataclass
class ObservedOnlyRegressor:
    """LightGBM Poisson regression fitted to uncensored spells only.

    Included to quantify the cost of the obvious shortcut: dropping the 22%
    of spells whose return was never seen removes most of the long ones, and
    the fitted model inherits that.
    """

    numeric: list[str]
    categorical: list[str]
    name: str = "lgbm_observed_only"
    params: dict = field(default_factory=dict)
    _model: object | None = None

    def _frame(self, rows: pl.DataFrame):
        df = rows.select(self.numeric + self.categorical).to_pandas()
        for c in self.categorical:
            df[c] = df[c].astype("category")
        for c in self.numeric:
            if df[c].dtype == bool:
                df[c] = df[c].astype(float)
        return df

    def fit(self, spells: pl.DataFrame) -> "ObservedOnlyRegressor":
        import lightgbm as lgb

        obs = spells.filter(pl.col("event") == 1)
        self._model = lgb.LGBMRegressor(
            objective="poisson", random_state=0, verbose=-1, n_jobs=-1, **self.params
        )
        self._model.fit(
            self._frame(obs),
            obs["games_missed"].to_numpy(),
            categorical_feature=self.categorical,
        )
        return self

    def predict(self, spells: pl.DataFrame) -> np.ndarray:
        return self._model.predict(self._frame(spells))

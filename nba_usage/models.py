"""Model runners, all scored on the same folds as the baselines."""

from __future__ import annotations

import numpy as np
import polars as pl

from .evaluate import metrics


def design_matrix(df: pl.DataFrame, cols: list[str]) -> np.ndarray:
    """Numeric matrix for sklearn/LightGBM, booleans cast to 0/1."""
    return (
        df.select([pl.col(c).cast(pl.Float64) for c in cols])
        .to_numpy()
        .astype(np.float64)
    )


def fit_lightgbm(
    train: pl.DataFrame,
    valid: pl.DataFrame,
    cols: list[str],
    target: str = "usg_pct",
    params: dict | None = None,
    num_boost_round: int = 2000,
) -> tuple[dict[str, float], object]:
    import lightgbm as lgb

    params = {
        "objective": "l2",
        "learning_rate": 0.05,
        "num_leaves": 63,
        "min_data_in_leaf": 100,
        "feature_fraction": 0.8,
        "bagging_fraction": 0.8,
        "bagging_freq": 1,
        "lambda_l2": 1.0,
        "verbose": -1,
        "seed": 0,
        **(params or {}),
    }
    # The last season of training is held out to pick the tree count, so the
    # validation season is never used for early stopping.
    seasons = sorted(train["season"].unique().to_list())
    inner_valid_season = seasons[-1]
    inner_tr = train.filter(pl.col("season") != inner_valid_season)
    inner_va = train.filter(pl.col("season") == inner_valid_season)

    dtrain = lgb.Dataset(design_matrix(inner_tr, cols), label=inner_tr[target].to_numpy(), feature_name=cols)
    dvalid = lgb.Dataset(design_matrix(inner_va, cols), label=inner_va[target].to_numpy(), reference=dtrain)
    booster = lgb.train(
        params,
        dtrain,
        num_boost_round=num_boost_round,
        valid_sets=[dvalid],
        callbacks=[lgb.early_stopping(100, verbose=False)],
    )
    pred = booster.predict(design_matrix(valid, cols), num_iteration=booster.best_iteration)
    return metrics(valid[target].to_numpy(), np.asarray(pred)), booster


def fit_ridge(
    train: pl.DataFrame,
    valid: pl.DataFrame,
    cols: list[str],
    target: str = "usg_pct",
    alpha: float = 1.0,
) -> tuple[dict[str, float], object]:
    from sklearn.impute import SimpleImputer
    from sklearn.linear_model import Ridge
    from sklearn.pipeline import make_pipeline
    from sklearn.preprocessing import StandardScaler

    model = make_pipeline(
        SimpleImputer(strategy="median"),
        StandardScaler(),
        Ridge(alpha=alpha),
    )
    model.fit(design_matrix(train, cols), train[target].to_numpy())
    pred = model.predict(design_matrix(valid, cols))
    return metrics(valid[target].to_numpy(), np.asarray(pred)), model

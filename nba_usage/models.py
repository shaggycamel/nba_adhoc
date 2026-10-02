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


def fit_elasticnet(
    train: pl.DataFrame, valid: pl.DataFrame, cols: list[str], target: str = "usg_pct"
) -> tuple[dict[str, float], object]:
    from sklearn.impute import SimpleImputer
    from sklearn.linear_model import ElasticNetCV
    from sklearn.pipeline import make_pipeline
    from sklearn.preprocessing import StandardScaler

    # The CV here is over the training rows only; the validation season is
    # never seen. Folds are contiguous blocks, not shuffled.
    from sklearn.model_selection import TimeSeriesSplit

    model = make_pipeline(
        SimpleImputer(strategy="median"),
        StandardScaler(),
        ElasticNetCV(l1_ratio=[0.1, 0.5, 0.9, 1.0], cv=TimeSeriesSplit(3), random_state=0, max_iter=5000),
    )
    model.fit(design_matrix(train, cols), train[target].to_numpy())
    pred = model.predict(design_matrix(valid, cols))
    return metrics(valid[target].to_numpy(), np.asarray(pred)), model


def _forest(kind: str):
    from sklearn.ensemble import ExtraTreesRegressor, RandomForestRegressor

    cls = RandomForestRegressor if kind == "rf" else ExtraTreesRegressor
    return cls(
        n_estimators=300,
        min_samples_leaf=20,
        max_features=0.4,
        n_jobs=-1,
        random_state=0,
    )


def fit_forest(
    train: pl.DataFrame,
    valid: pl.DataFrame,
    cols: list[str],
    target: str = "usg_pct",
    kind: str = "rf",
) -> tuple[dict[str, float], object]:
    from sklearn.impute import SimpleImputer
    from sklearn.pipeline import make_pipeline

    model = make_pipeline(SimpleImputer(strategy="median"), _forest(kind))
    model.fit(design_matrix(train, cols), train[target].to_numpy())
    pred = model.predict(design_matrix(valid, cols))
    return metrics(valid[target].to_numpy(), np.asarray(pred)), model


def fit_xgboost(
    train: pl.DataFrame, valid: pl.DataFrame, cols: list[str], target: str = "usg_pct"
) -> tuple[dict[str, float], object]:
    import xgboost as xgb

    seasons = sorted(train["season"].unique().to_list())
    inner_tr = train.filter(pl.col("season") != seasons[-1])
    inner_va = train.filter(pl.col("season") == seasons[-1])

    model = xgb.XGBRegressor(
        n_estimators=2000,
        learning_rate=0.05,
        max_depth=6,
        subsample=0.8,
        colsample_bytree=0.8,
        reg_lambda=1.0,
        min_child_weight=20,
        early_stopping_rounds=100,
        n_jobs=-1,
        random_state=0,
    )
    model.fit(
        design_matrix(inner_tr, cols),
        inner_tr[target].to_numpy(),
        eval_set=[(design_matrix(inner_va, cols), inner_va[target].to_numpy())],
        verbose=False,
    )
    pred = model.predict(design_matrix(valid, cols))
    return metrics(valid[target].to_numpy(), np.asarray(pred)), model


def fit_catboost(
    train: pl.DataFrame, valid: pl.DataFrame, cols: list[str], target: str = "usg_pct"
) -> tuple[dict[str, float], object]:
    from catboost import CatBoostRegressor, Pool

    seasons = sorted(train["season"].unique().to_list())
    inner_tr = train.filter(pl.col("season") != seasons[-1])
    inner_va = train.filter(pl.col("season") == seasons[-1])

    model = CatBoostRegressor(
        iterations=3000,
        learning_rate=0.05,
        depth=6,
        l2_leaf_reg=3.0,
        loss_function="RMSE",
        random_seed=0,
        verbose=False,
        early_stopping_rounds=100,
    )
    model.fit(
        Pool(design_matrix(inner_tr, cols), inner_tr[target].to_numpy()),
        eval_set=Pool(design_matrix(inner_va, cols), inner_va[target].to_numpy()),
    )
    pred = model.predict(design_matrix(valid, cols))
    return metrics(valid[target].to_numpy(), np.asarray(pred)), model

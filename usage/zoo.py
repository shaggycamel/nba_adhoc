"""Classical-ML model comparison on identical time-based folds (all feature groups)."""
import itertools
import json
import sys
import time

import catboost as cb
import lightgbm as lgb
import numpy as np
import polars as pl
import xgboost as xgb
from sklearn.ensemble import ExtraTreesRegressor, RandomForestRegressor
from sklearn.linear_model import ElasticNet, Ridge
from sklearn.preprocessing import StandardScaler

from . import data as D
from .gbm import PARAMS, prepared_folds

FEATS = D.cols(D.ORDER)
PRED_DIR = D.CACHE / "preds"
PRED_DIR.mkdir(exist_ok=True)


def matrices(tr, va, impute):
    Xtr, Xva = tr.select(FEATS).to_numpy().astype(np.float32), va.select(FEATS).to_numpy().astype(np.float32)
    if not impute:
        return Xtr, Xva
    med = np.nanmedian(Xtr, axis=0)
    ind = [FEATS.index("own_sev")]
    mtr, mva = np.isnan(Xtr[:, ind]).astype(np.float32), np.isnan(Xva[:, ind]).astype(np.float32)
    Xtr, Xva = np.where(np.isnan(Xtr), med, Xtr), np.where(np.isnan(Xva), med, Xva)
    sc = StandardScaler().fit(Xtr)
    return np.hstack([np.clip(sc.transform(Xtr), -6, 6), mtr]), np.hstack([np.clip(sc.transform(Xva), -6, 6), mva])


def lgb_make(**kw):
    def f(Xtr, ytr, Xva, yva):
        m = lgb.LGBMRegressor(**{**PARAMS, **kw})
        m.fit(Xtr, ytr)
        return m.predict(Xva)
    return f


def xgb_make(**kw):
    def f(Xtr, ytr, Xva, yva):
        m = xgb.XGBRegressor(tree_method="hist", n_estimators=600, learning_rate=0.03, subsample=0.8,
                             colsample_bytree=0.8, min_child_weight=50, reg_lambda=5.0, n_jobs=4, **kw)
        m.fit(Xtr, ytr)
        return m.predict(Xva)
    return f


def cb_make(**kw):
    def f(Xtr, ytr, Xva, yva):
        m = cb.CatBoostRegressor(iterations=800, learning_rate=0.06, l2_leaf_reg=5, verbose=0, thread_count=4,
                                 random_seed=0, **kw)
        m.fit(Xtr, ytr)
        return m.predict(Xva)
    return f


def sk_make(cls, **kw):
    def f(Xtr, ytr, Xva, yva):
        m = cls(**kw)
        m.fit(Xtr, ytr)
        return m.predict(Xva)
    return f


MODELS = {
    # name: (impute, [(label, factory)])
    "Ridge": (True, [(f"alpha={a}", sk_make(Ridge, alpha=a)) for a in (1, 100, 1000)]),
    "ElasticNet": (True, [(f"alpha={a}", sk_make(ElasticNet, alpha=a, l1_ratio=0.5, max_iter=2000))
                          for a in (1e-4, 1e-3)]),
    "RandomForest": (True, [(f"leaf={l}", sk_make(RandomForestRegressor, n_estimators=150, min_samples_leaf=l,
                                                  max_features=0.4, max_samples=0.3, n_jobs=4, random_state=0))
                            for l in (30, 100)]),
    "ExtraTrees": (True, [(f"leaf={l}", sk_make(ExtraTreesRegressor, n_estimators=150, min_samples_leaf=l,
                                                max_features=0.5, max_samples=0.3, bootstrap=True, n_jobs=4,
                                                random_state=0)) for l in (30, 100)]),
    "LightGBM": (False, [(f"leaves={nl},child={mc}", lgb_make(num_leaves=nl, min_child_samples=mc))
                         for nl, mc in itertools.product((15, 31, 63), (50, 200))]),
    "XGBoost": (False, [(f"depth={d}", xgb_make(max_depth=d)) for d in (4, 6, 8)]),
    "CatBoost": (False, [(f"depth={d}", cb_make(depth=d)) for d in (6, 8)]),
}
LOSSES = {  # LightGBM objective comparison at the tuned shape
    "L2 (squared error)": dict(objective="regression"),
    "L1 (absolute error)": dict(objective="regression_l1"),
    "Huber (delta=0.05)": dict(objective="huber", alpha=0.05),
    "Fair (c=0.05)": dict(objective="fair", fair_c=0.05),
}


def run(only=None):
    df, a = D.load()
    folds = list(prepared_folds(df, a))
    res = json.load(open(D.CACHE / "zoo.json")) if (D.CACHE / "zoo.json").exists() else {}
    keys = {n: (tr["game_id"].to_numpy(), tr["player_id"].to_numpy()) for n, tr, va in folds}
    for name, (imp, grid) in MODELS.items():
        if only and name not in only:
            continue
        t = time.time()
        mats = [(n, *matrices(tr, va, imp), tr["usg_pct"].to_numpy(), va["usg_pct"].to_numpy()) for n, tr, va in folds]
        rows = []
        for label, fac in grid:
            per = []
            for n, Xtr, Xva, ytr, yva in mats:
                p = fac(Xtr, ytr, Xva, yva)
                per.append({"fold": n, **D.metrics(yva, p)})
                np.save(PRED_DIR / f"{name}__{label}__{n}.npy", p)
            rows.append({"config": label, "res": per})
            print(f"{name:12s} {label:22s}", " ".join(f"{r['mae']:.4f}" for r in per), f"{time.time()-t:.0f}s", flush=True)
        res[name] = rows
        json.dump(res, open(D.CACHE / "zoo.json", "w"))
    json.dump({n: {"game_id": va["game_id"].to_list(), "player_id": va["player_id"].to_list(),
                   "y": va["usg_pct"].to_list()} for n, tr, va in folds}, open(D.CACHE / "val_keys.json", "w"))


def losses():
    df, a = D.load()
    folds = list(prepared_folds(df, a))
    zoo = json.load(open(D.CACHE / "zoo.json"))
    best = min(zoo["LightGBM"], key=lambda r: np.mean([x["mae"] for x in r["res"][:3]]))["config"]
    nl, mc = [int(s.split("=")[1]) for s in best.split(",")]
    out = {}
    for lname, kw in LOSSES.items():
        per = []
        for n, tr, va in folds:
            m = lgb.LGBMRegressor(**{**PARAMS, "num_leaves": nl, "min_child_samples": mc, **kw})
            m.fit(tr.select(FEATS).to_numpy(), tr["usg_pct"].to_numpy())
            p = m.predict(va.select(FEATS).to_numpy())
            np.save(PRED_DIR / f"LGB_loss_{lname[:2]}__{n}.npy", p)
            per.append({"fold": n, **D.metrics(va["usg_pct"].to_numpy(), p)})
        out[lname] = per
        print(f"{lname:22s}", " ".join(f"{r['mae']:.4f}" for r in per), flush=True)
    json.dump(out, open(D.CACHE / "losses.json", "w"))


if __name__ == "__main__":
    if sys.argv[1:] == ["losses"]:
        losses()
    else:
        run(sys.argv[1:] or None)

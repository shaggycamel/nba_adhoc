import json
import sys
import time

import lightgbm as lgb
import numpy as np
import polars as pl

from . import data as D

PARAMS = dict(objective="regression", learning_rate=0.03, num_leaves=31, min_child_samples=100, subsample=0.8,
              subsample_freq=1, colsample_bytree=0.8, reg_lambda=5.0, n_estimators=600, verbose=-1, n_jobs=4)


def prepared_folds(df, a, with_roles=True):
    """Yield (fold name, train, val) frames with fold-fitted role features attached."""
    for name, tb, v in D.FOLDS:
        tr, va = D.split(df, tb, v)
        if with_roles:
            roles = D.fit_roles(tr)
            tr, va = D.add_roles(tr, a, roles), D.add_roles(va, a, roles)
        yield name, tr, va


def fit_predict(tr, va, feats, **over):
    prm = {**PARAMS, **over}
    m = lgb.LGBMRegressor(**prm)
    m.fit(tr.select(feats).to_numpy(), tr["usg_pct"].to_numpy())
    return m, m.predict(va.select(feats).to_numpy())


def evaluate(folds, feats, **over):
    res = []
    for name, tr, va in folds:
        _, p = fit_predict(tr, va, feats, **over)
        res.append({"fold": name, **D.metrics(va["usg_pct"].to_numpy(), p)})
    return res


def cv_mae(res):
    return float(np.mean([r["mae"] for r in res[:3]]))


def ablations():
    df, a = D.load()
    folds = list(prepared_folds(df, a))
    out = {"forward": [], "leave_one_out": []}
    t = time.time()
    used = []
    for g in D.ORDER:
        used.append(g)
        r = evaluate(folds, D.cols(used))
        out["forward"].append({"added": g, "groups": list(used), "res": r})
        print(f"+{g:12s}", " ".join(f"{x['mae']:.4f}" for x in r), f"{time.time()-t:.0f}s", flush=True)
    for g in D.ORDER:
        keep = [x for x in D.ORDER if x != g]
        r = evaluate(folds, D.cols(keep))
        out["leave_one_out"].append({"dropped": g, "res": r})
        print(f"-{g:12s}", " ".join(f"{x['mae']:.4f}" for x in r), f"{time.time()-t:.0f}s", flush=True)
    json.dump(out, open(D.CACHE / "ablation.json", "w"))


if __name__ == "__main__":
    ablations()

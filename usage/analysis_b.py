"""LightGBM ablations with saved predictions (paired CIs), seed noise, importance, top-k curve."""
import json
import time

import lightgbm as lgb
import numpy as np
import polars as pl

from . import data as D
from .common import cluster_boot, lgb_over
from .gbm import PARAMS, prepared_folds

OUT = D.CACHE / "analysis_b.json"


def main():
    df, a = D.load()
    folds = list(prepared_folds(df, a))
    over = lgb_over()
    ytrue = {n: va["usg_pct"].to_numpy() for n, tr, va in folds}
    gids = {n: va["game_id"].to_numpy() for n, tr, va in folds}
    res, t0 = {}, time.time()

    def preds(feats, seed=0, n_est=None, lr=None, tag=""):
        out = {}
        for n, tr, va in folds:
            kw = {**PARAMS, **over, "random_state": seed}
            if n_est:
                kw["n_estimators"], kw["learning_rate"] = n_est, lr
            m = lgb.LGBMRegressor(**kw).fit(tr.select(feats).to_numpy(), tr["usg_pct"].to_numpy())
            out[n] = m.predict(va.select(feats).to_numpy())
        return out

    def mae(p):
        return {n: float(np.abs(p[n] - ytrue[n]).mean()) for n in p}

    def paired(p, q):
        """mean (|p-y| - |q-y|) pooled over all folds with game-cluster CI; negative => p better."""
        d = np.concatenate([np.abs(p[n] - ytrue[n]) - np.abs(q[n] - ytrue[n]) for n in p])
        g = np.concatenate([gids[n] for n in p])
        m, lo, hi = cluster_boot(d, g)
        dh = np.abs(p[folds[-1][0]] - ytrue[folds[-1][0]]) - np.abs(q[folds[-1][0]] - ytrue[folds[-1][0]])
        mh, loh, hih = cluster_boot(dh, gids[folds[-1][0]])
        return {"pooled": [m, lo, hi], "holdout": [mh, loh, hih]}

    full = preds(D.cols(D.ORDER))
    res["full"] = {"mae": mae(full)}
    print("full", res["full"], f"{time.time()-t0:.0f}s", flush=True)
    seeds = {s: preds(D.cols(D.ORDER), seed=s) for s in (1, 2)}
    res["seed_noise"] = {str(s): mae(p) for s, p in seeds.items()}
    res["seed_pair"] = paired(seeds[1], full)
    core = preds(D.cols(["core"]))
    res["core"] = {"mae": mae(core), "vs_full": paired(core, full)}
    res["add_to_core"], res["leave_one_out"] = {}, {}
    for g in D.ORDER[1:]:
        p = preds(D.cols(["core", g]))
        res["add_to_core"][g] = {"mae": mae(p), "vs_core": paired(p, core)}
        print("core+", g, res["add_to_core"][g]["vs_core"]["pooled"], f"{time.time()-t0:.0f}s", flush=True)
    for g in D.ORDER:
        p = preds(D.cols([x for x in D.ORDER if x != g]))
        res["leave_one_out"][g] = {"mae": mae(p), "vs_full": paired(p, full)}
        print("full-", g, res["leave_one_out"][g]["vs_full"]["pooled"], f"{time.time()-t0:.0f}s", flush=True)
    # injury-only block removals
    for name, drop in {"all injury-derived": ["own_injury", "team_injury", "hierarchy", "pairwise", "roles"],
                       "team-level injury (no own)": ["team_injury", "hierarchy", "pairwise", "roles"]}.items():
        p = preds(D.cols([x for x in D.ORDER if x not in drop]))
        res.setdefault("block_removal", {})[name] = {"mae": mae(p), "vs_full": paired(p, full)}
        print("block", name, res["block_removal"][name]["vs_full"]["pooled"], flush=True)
    json.dump(res, open(OUT, "w"))

    # importance on the full model: gain, SHAP, group permutation, per fold
    feats = D.cols(D.ORDER)
    imp = {"gain": {}, "shap": {}, "perm": {}}
    rng = np.random.default_rng(0)
    topk, ks = {}, [1, 2, 3, 5, 8, 12, 20, 30, 45, len(feats)]
    for n, tr, va in folds:
        m = lgb.LGBMRegressor(**{**PARAMS, **over, "importance_type": "gain"}).fit(
            tr.select(feats).to_numpy(), tr["usg_pct"].to_numpy())
        gain = m.feature_importances_ / m.feature_importances_.sum()
        imp["gain"][n] = dict(zip(feats, map(float, gain)))
        sub = va.sample(min(40000, va.height), seed=0)
        Xs = sub.select(feats).to_numpy()
        sh = m.predict(Xs, pred_contrib=True)[:, :-1]
        imp["shap"][n] = dict(zip(feats, map(float, np.abs(sh).mean(0))))
        base_mae = float(np.abs(m.predict(Xs) - sub["usg_pct"].to_numpy()).mean())
        pg = {}
        for g in D.ORDER:
            ix = [feats.index(c) for c in D.cols([g])]
            dm = []
            for _ in range(3):
                Xp = Xs.copy()
                Xp[:, ix] = Xp[rng.permutation(len(Xp))][:, ix]
                dm.append(float(np.abs(m.predict(Xp) - sub["usg_pct"].to_numpy()).mean()) - base_mae)
            pg[g] = float(np.mean(dm))
        imp["perm"][n] = pg
        # smallest subset: rank by this fold's training-set gain, refit with top-k
        order = [feats[i] for i in np.argsort(-gain)]
        topk[n] = {}
        for k in ks:
            f = order[:k]
            mk = lgb.LGBMRegressor(**{**PARAMS, **over, "n_estimators": 300, "learning_rate": 0.06}).fit(
                tr.select(f).to_numpy(), tr["usg_pct"].to_numpy())
            topk[n][k] = {"mae": float(np.abs(mk.predict(va.select(f).to_numpy()) - ytrue[n]).mean()),
                          "feats": f if k <= 12 else None}
        print("importance fold", n, f"{time.time()-t0:.0f}s", flush=True)
    res["importance"], res["topk"] = imp, {n: {str(k): v for k, v in d.items()} for n, d in topk.items()}
    json.dump(res, open(OUT, "w"))
    print("done")


if __name__ == "__main__":
    main()

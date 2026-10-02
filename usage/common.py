import json

import numpy as np

from . import data as D


def best_cfg(model="LightGBM"):
    zoo = json.load(open(D.CACHE / "zoo.json"))
    row = min(zoo[model], key=lambda r: np.mean([x["mae"] for x in r["res"][:3]]))
    return row["config"], row


def lgb_over():
    label, _ = best_cfg("LightGBM")
    nl, mc = [int(s.split("=")[1]) for s in label.split(",")]
    return {"num_leaves": nl, "min_child_samples": mc}


def cluster_boot(diff, groups, B=1000, seed=0):
    """Mean of per-row `diff` with a 95% CI from resampling whole games (clusters)."""
    diff, groups = np.asarray(diff, float), np.asarray(groups)
    u, inv = np.unique(groups, return_inverse=True)
    s = np.bincount(inv, weights=diff)
    c = np.bincount(inv).astype(float)
    rng = np.random.default_rng(seed)
    idx = rng.integers(0, len(u), size=(B, len(u)))
    means = s[idx].sum(1) / c[idx].sum(1)
    return float(diff.mean()), float(np.percentile(means, 2.5)), float(np.percentile(means, 97.5))

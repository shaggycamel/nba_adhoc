"""Assemble all result JSONs into one data blob and render REPORT.html from the template."""
import collections
import json
import re
from pathlib import Path

import numpy as np

from . import data as D

ROOT = Path(__file__).resolve().parent.parent
C = D.CACHE
FAMILY = {"Ridge": "Linear", "ElasticNet": "Linear", "RandomForest": "Forest", "ExtraTrees": "Forest",
          "LightGBM": "Boosting", "XGBoost": "Boosting", "CatBoost": "Boosting", "MLP": "Neural", "GRU": "Neural"}
FN = [f[0] for f in D.FOLDS]


def times():
    """Average fit time per configuration over all four folds (seconds)."""
    last, count = {}, collections.Counter()
    for line in open(C / "zoo.log"):
        m = re.match(r"^(\w+)\s+(.+?)\s+((?:0\.\d{4}\s+){4})(\d+)s$", line.strip())
        if m:
            last[m.group(1)] = float(m.group(4))
            count[m.group(1)] += 1
    t = {(k, "*"): last[k] / count[k] for k in last}
    for fn, nm in (("nn_mlp.log", "MLP"), ("nn_gru.log", "GRU")):
        cur = {}
        for line in open(C / fn):
            m = re.match(r"^(MLP|GRU) (\w+) (.+?) mae=[\d.]+ (\d+)s", line.strip())
            if m:
                cur[m.group(2)] = float(m.group(4))
        for k, v in cur.items():
            t[(nm, k)] = v
    return t


def main():
    a = json.load(open(C / "analysis_a.json"))
    b = json.load(open(C / "analysis_b.json"))
    c = json.load(open(C / "analysis_c.json"))
    tm = times()
    df, _ = D.load()
    folds = []
    for n, tb, v in D.FOLDS:
        tr, va = D.split(df, tb, v)
        folds.append({"name": n, "train": tr.height, "val": va.height})
    models = []
    for name, m in a["models"].items():
        base = name.split(" ")[0]
        res = m["res"]
        key = (base, "*") if base in FAMILY and base not in ("MLP", "GRU") else (base, m["config"])
        models.append({"name": name, "family": FAMILY.get(base, "Other"), "config": m["config"],
                       "mae": [r["mae"] for r in res], "rmse_h": res[3]["rmse"], "r2_h": res[3]["r2"],
                       "seconds": tm.get(key), "grid": m.get("grid")})
    simple = [{"name": k, "mae": [r["mae"] for r in v], "r2_h": v[3]["r2"]} for k, v in a["baselines"].items()]
    mw = a["paired_mw"]
    simple.append({"name": "minutes-wtd EWMA a=0.1", "mae": [mw["mw_mae"][n] for n in FN],
                   "r2_h": mw["mw_metrics"][FN[3]]["r2"]})
    imp = b["importance"]
    agg = {k: collections.defaultdict(float) for k in ("gain", "shap")}
    for kind in agg:
        for f, d in imp[kind].items():
            for k, v in d.items():
                agg[kind][k] += v / len(imp[kind])
    top = sorted(agg["shap"].items(), key=lambda x: -x[1])[:16]
    group_of = {f: g for g, fs in D.GROUPS.items() for f in fs}
    perm = {g: float(np.mean([imp["perm"][f][g] for f in imp["perm"]])) for g in D.ORDER}
    topk = {}
    for k in b["topk"][FN[0]]:
        topk[k] = [b["topk"][f][k]["mae"] for f in FN]
    best = next(m for m in models if m["name"] == "LightGBM")
    pm = {(p["a"], p["b"]): p for p in a["paired"]}
    K = {
        "best_hold": best["mae"][3], "best_r2": best["r2_h"], "mw_hold": mw["mw_mae"][FN[3]],
        "mw_r2": mw["mw_metrics"][FN[3]]["r2"], "gain_hold": -mw["holdout"][0], "gain_lo": -mw["holdout"][2],
        "gain_hi": -mw["holdout"][1], "gain_pct": -mw["holdout"][0] / mw["mw_mae"][FN[3]] * 100,
        "gain_pool": -mw["pooled"][0], "season_hold": next(s for s in simple if s["name"] == "season mean")["mae"][3],
        "inj_gain": b["block_removal"]["all injury-derived"]["vs_full"]["pooled"][0],
        "inj_lo": b["block_removal"]["all injury-derived"]["vs_full"]["pooled"][1],
        "inj_hi": b["block_removal"]["all injury-derived"]["vs_full"]["pooled"][2],
        "inj_noown": b["block_removal"]["team-level injury (no own)"]["vs_full"]["pooled"][0],
        "own_loo": b["leave_one_out"]["own_injury"]["vs_full"]["pooled"],
        "core_gap": b["core"]["vs_full"]["pooled"][0],
        "noise_sd": a["noise"]["within_player_season_sd"], "all_sd": a["noise"]["overall_sd"],
        "resid_sd": a["residual_hist"]["sd"], "own_out_played": a["coverage"]["own_out_but_played"],
        "rows": a["coverage"]["rows_total"], "rows_inj": a["coverage"]["rows_injury_era"],
        "top5_hold": topk["5"][3], "top12_hold": topk["12"][3], "all_hold": topk[max(topk, key=int)][3],
        "n_feats": int(max(topk, key=int)), "seed_sd": float(np.std([b["full"]["mae"][FN[3]],
                                                                     b["seed_noise"]["1"][FN[3]],
                                                                     b["seed_noise"]["2"][FN[3]]])),
        "statyx": c["statyx"], "processed": c["processed"]["paired"],
    }
    data = {
        "K": K, "folds": folds, "foldNames": FN, "models": models, "simple": simple,
        "ensembles": a["ensembles"], "paired": a["paired"], "paired_mw": {k: mw[k] for k in ("pooled", "holdout")},
        "losses": a["losses"],
        "curves": {nm: a["models"][nm]["curves"] for nm in ("MLP (huber)", "GRU (huber)", "MLP (mse)", "MLP (l1)")},
        "ablation": {"add": b["add_to_core"], "loo": b["leave_one_out"], "block": b["block_removal"],
                     "core": b["core"], "seed_pair": b["seed_pair"], "order": D.ORDER},
        "importance": {"shap": [{"f": k, "v": v, "g": group_of[k], "gain": agg["gain"][k]} for k, v in top],
                       "perm": perm, "topk": topk,
                       "k5": b["topk"][FN[3]]["5"]["feats"], "k12": b["topk"][FN[3]]["12"]["feats"]},
        "groups": {g: len(fs) for g, fs in D.GROUPS.items()},
        "inj": {k: a[k] for k in ("own_status", "vacated_curve", "stale_curve", "rank_effect", "return_gap")},
        "segments": a["segments"], "calibration": a["calibration"], "residual": a["residual_hist"],
        "processing": a["processing"], "processedFeat": c["processed"], "statyx": c["statyx"],
    }
    tpl = (ROOT / "usage" / "report_template.html").read_text()
    html = tpl.replace("/*DATA*/null", json.dumps(data, default=float))
    (ROOT / "REPORT.html").write_text(html)
    print("wrote REPORT.html", len(html) // 1024, "KB")


if __name__ == "__main__":
    main()

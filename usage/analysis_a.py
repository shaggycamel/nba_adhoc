"""Model summary, paired tests, ensemble, descriptive injury effects, processing variants (no training)."""
import json
import re

import numpy as np
import polars as pl

from . import data as D
from . import features as F
from .common import best_cfg, cluster_boot

P = D.CACHE / "preds"
FN = [f[0] for f in D.FOLDS]
BASE = "usg_ew1"


def main():
    df, a = D.load()
    val = {n: D.split(df, tb, v)[1] for n, tb, v in D.FOLDS}
    y = {n: val[n]["usg_pct"].to_numpy() for n in FN}
    gid = {n: val[n]["game_id"].to_numpy() for n in FN}
    zoo = json.load(open(D.CACHE / "zoo.json"))
    nn = json.load(open(D.CACHE / "nn.json"))
    losses = json.load(open(D.CACHE / "losses.json"))
    bl = json.load(open(D.CACHE / "baselines.json"))
    out = {"folds": FN}

    # --- model table: best config per algorithm chosen on the three validation folds only
    models, preds = {}, {}
    for name in zoo:
        label, row = best_cfg(name)
        models[name] = {"config": label, "res": row["res"], "grid": [
            {"config": r["config"], "cv": float(np.mean([x["mae"] for x in r["res"][:3]])),
             "holdout": r["res"][3]["mae"]} for r in zoo[name]]}
        preds[name] = {n: np.load(P / f"{name}__{label}__{n}.npy") for n in FN}
    for key, v in nn.items():
        m, kind = key.split("|")
        nm = f"{m} ({kind})"
        models[nm] = {"config": kind, "res": v["res"], "curves": v["curves"]}
        preds[nm] = {n: np.load(P / f"{m}__{kind}__{n}.npy") for n in FN}
    out["models"] = models
    out["baselines"] = bl
    out["losses"] = {"LightGBM": losses, "MLP": {
        k.split("|")[1]: v["res"] for k, v in nn.items() if k.startswith("MLP")}}

    # baseline arrays: best simple baseline is chosen on validation folds too
    def base_pred(col, n):
        tr = D.split(df, dict((f[0], f[1]) for f in D.FOLDS)[n] if False else
                     [f for f in D.FOLDS if f[0] == n][0][1], "x")[0] if False else None
        fb = df.filter(pl.col("season") < [f for f in D.FOLDS if f[0] == n][0][1])["usg_pct"].mean()
        return val[n].select(pl.coalesce(col, "usg_season", "usg_career", pl.lit(fb)))[:, 0].to_numpy()
    bp = {n: base_pred("usg_ew1", n) for n in FN}
    sp = {n: base_pred("usg_season", n) for n in FN}
    preds["EWMA baseline"] = bp
    preds["Season-mean baseline"] = sp

    # --- ensemble of the best of each family
    fam = ["Ridge", "LightGBM", "XGBoost", "CatBoost", "RandomForest", "MLP (huber)", "GRU (huber)"]
    preds["Ensemble (mean of 7)"] = {n: np.mean([preds[f][n] for f in fam], axis=0) for n in FN}
    gb3 = ["LightGBM", "XGBoost", "CatBoost"]
    preds["Ensemble (GBMs)"] = {n: np.mean([preds[f][n] for f in gb3], axis=0) for n in FN}
    ens = {}
    for k in ("Ensemble (mean of 7)", "Ensemble (GBMs)"):
        ens[k] = [{"fold": n, **D.metrics(y[n], preds[k][n])} for n in FN]
    out["ensembles"] = ens

    # --- paired tests (pooled folds and holdout): |err_a| - |err_b|, negative => a better
    def pair(a_, b_):
        d = {n: np.abs(preds[a_][n] - y[n]) - np.abs(preds[b_][n] - y[n]) for n in FN}
        pooled = cluster_boot(np.concatenate(list(d.values())), np.concatenate([gid[n] for n in FN]))
        hold = cluster_boot(d[FN[-1]], gid[FN[-1]])
        return {"a": a_, "b": b_, "pooled": list(pooled), "holdout": list(hold)}
    comps = [("LightGBM", "EWMA baseline"), ("LightGBM", "Season-mean baseline"), ("LightGBM", "Ridge"),
             ("LightGBM", "XGBoost"), ("LightGBM", "CatBoost"), ("LightGBM", "RandomForest"),
             ("LightGBM", "MLP (huber)"), ("LightGBM", "GRU (huber)"), ("MLP (huber)", "GRU (huber)"),
             ("Ridge", "EWMA baseline"), ("Ensemble (mean of 7)", "LightGBM"), ("EWMA baseline", "Season-mean baseline")]
    out["paired"] = [pair(*c) for c in comps]

    # --- where does the model beat the baseline? (LightGBM vs EWMA baseline, pooled folds)
    allv = pl.concat([val[n].with_columns(pl.Series("gbm", preds["LightGBM"][n]), pl.Series("ewma", bp[n]),
                                          pl.Series("fold", [n] * val[n].height)) for n in FN])
    allv = allv.with_columns((pl.col("gbm") - pl.col("usg_pct")).abs().alias("e_gbm"),
                             (pl.col("ewma") - pl.col("usg_pct")).abs().alias("e_ewma"),
                             (pl.col("usg_pct") - pl.col("ewma")).alias("delta"))
    def seg(expr_col, bins, labels):
        q = allv.with_columns(pl.col(expr_col).cut(bins, labels=labels).alias("seg"))
        return (q.group_by("seg").agg(pl.len().alias("n"), pl.col("e_gbm").mean().alias("gbm"),
                                       pl.col("e_ewma").mean().alias("ewma"), pl.col("delta").mean().alias("delta"))
                .sort("seg").to_dicts())
    out["segments"] = {
        "vacated usage by fresh Out teammates": seg("t_vac_out_fresh", [1e-9, 0.05, 0.15, 0.3],
                                                     ["none", "<0.05", "0.05-0.15", "0.15-0.30", ">0.30"]),
        "minutes tier (last 10)": seg("min_l10", [15, 22, 28, 33], ["<15", "15-22", "22-28", "28-33", "33+"]),
        "games of history": seg("n_games", [5, 20, 60, 150], ["<5", "5-20", "20-60", "60-150", "150+"]),
        "days since last game": seg("rest_days", [1.5, 3.5, 7.5, 14.5], ["1", "2-3", "4-7", "8-14", "15+"]),
        "usage level (EWMA)": seg("ewma", [0.12, 0.17, 0.22, 0.27], ["<.12", ".12-.17", ".17-.22", ".22-.27", ".27+"]),
    }
    # calibration on pooled validation: deciles of prediction
    cal = (allv.with_columns(pl.col("gbm").qcut(10, labels=[str(i) for i in range(10)]).alias("d"))
           .group_by("d").agg(pl.col("gbm").mean().alias("pred"), pl.col("usg_pct").mean().alias("actual"),
                              pl.len().alias("n")).sort("d"))
    out["calibration"] = cal.to_dicts()
    res = (allv["gbm"] - allv["usg_pct"]).to_numpy()
    h, e = np.histogram(res, bins=np.linspace(-0.2, 0.2, 41))
    out["residual_hist"] = {"edges": e.tolist(), "counts": h.tolist(),
                            "sd": float(res.std()), "p10": float(np.percentile(res, 10)), "p90": float(np.percentile(res, 90))}
    # signal vs noise: how much of usage variance is game-to-game noise around a player-season mean
    ps = (df.group_by("player_id", "season").agg(pl.col("usg_pct").std().alias("sd"), pl.len().alias("n"))
          .filter(pl.col("n") >= 20))
    out["noise"] = {"within_player_season_sd": float(ps["sd"].mean()), "overall_sd": float(df["usg_pct"].std())}

    # --- injury descriptives (injury era rows, all folds' val rows + earlier injury-era train rows)
    inj = df.filter(pl.col("own_sev").is_not_null()).with_columns((pl.col("usg_pct") - pl.col("usg_l10")).alias("d10"))
    out["own_status"] = (inj.group_by("own_sev").agg(pl.len().alias("n"), pl.col("d10").mean().alias("delta"),
                                                    pl.col("min").mean().alias("min"),
                                                    (pl.col("d10").std() / pl.len().sqrt()).alias("se")).sort("own_sev")
                         .to_dicts())
    vb = inj.with_columns(pl.col("t_vac_out_fresh").cut([1e-9, 0.05, 0.1, 0.2, 0.3, 0.45],
                                                       labels=["0", "<.05", ".05-.1", ".1-.2", ".2-.3", ".3-.45", ">.45"])
                          .alias("b"))
    out["vacated_curve"] = (vb.group_by("b").agg(pl.len().alias("n"), pl.col("d10").mean().alias("delta"),
                                                (pl.col("d10").std() / pl.len().sqrt()).alias("se"))
                            .sort("b").to_dicts())
    sb = inj.with_columns(pl.col("t_vac_out_stale").cut([1e-9, 0.05, 0.1, 0.2, 0.3],
                                                       labels=["0", "<.05", ".05-.1", ".1-.2", ".2-.3", ">.3"]).alias("b"))
    out["stale_curve"] = (sb.group_by("b").agg(pl.len().alias("n"), pl.col("d10").mean().alias("delta"),
                                              (pl.col("d10").std() / pl.len().sqrt()).alias("se")).sort("b").to_dicts())
    hr = (inj.filter(pl.col("healthy_rank").is_not_null() & (pl.col("healthy_rank") <= 10))
          .with_columns((pl.col("t_vac_out_fresh") > 0.1).alias("star_out"))
          .group_by("healthy_rank", "star_out").agg(pl.len().alias("n"), pl.col("d10").mean().alias("delta"))
          .sort("healthy_rank", "star_out"))
    out["rank_effect"] = hr.to_dicts()
    # usage change by gap since last appearance (return from absence)
    gp = (df.with_columns((pl.col("usg_pct") - pl.col("usg_l10")).alias("d10"))
          .with_columns(pl.col("team_games_missed").cut([0.5, 2.5, 6.5, 15.5], labels=["0", "1-2", "3-6", "7-15", "16+"]).alias("g"))
          .group_by("g").agg(pl.len().alias("n"), pl.col("d10").mean().alias("delta"),
                             (pl.col("d10").std() / pl.len().sqrt()).alias("se")).sort("g"))
    out["return_gap"] = gp.to_dicts()
    # coverage of injury data
    out["coverage"] = {"rows_total": df.height, "rows_injury_era": inj.height,
                       "own_out_but_played": int(inj.filter(pl.col("own_sev") == 5).height)}

    # --- processing variants: single-number predictors on the same validation rows
    box, tg = F.load_games()
    p = F.player_history(box).select("player_id", "game_id", "game_date", "usg_pct", "min").sort(
        "player_id", "game_date", "game_id")
    cutoff = pl.date(2022, 10, 1)
    lo, hi = p.filter(pl.col("game_date") < cutoff)["usg_pct"].quantile(0.01), p.filter(
        pl.col("game_date") < cutoff)["usg_pct"].quantile(0.99)
    keys = pl.concat([val[n].select("player_id", "game_id", "game_date").with_columns(pl.lit(n).alias("fold"))
                      for n in FN])

    def variant(frame, expr, name):
        f = frame.with_columns(expr.over("player_id").alias("post")).select("player_id", "game_date", "post")
        j = (keys.with_row_index("ord").sort("game_date")
             .join_asof(f.sort("game_date"), on="game_date", by="player_id",
                        strategy="backward", allow_exact_matches=False).sort("ord"))
        return j["post"].to_numpy(), j["fold"].to_numpy()

    yk = pl.concat([val[n].select("usg_pct") for n in FN])["usg_pct"].to_numpy()
    fk = np.concatenate([[n] * val[n].height for n in FN])
    fall = np.concatenate([bp[n] for n in FN])

    def score(name, pred):
        pred = np.where(np.isnan(pred), fall, pred)
        r = {n: float(np.abs(pred[fk == n] - yk[fk == n]).mean()) for n in FN}
        return {"name": name, "cv": float(np.mean([r[n] for n in FN[:3]])), "holdout": r[FN[-1]]}

    u = pl.col("usg_pct")
    pv = {"windows": [], "alphas": [], "history": []}
    for w in (3, 5, 8, 10, 15, 20, 30, 40, 60):
        pr, _ = variant(p, u.rolling_mean(w, min_samples=1), f"w{w}")
        pv["windows"].append({**score(f"{w}", pr), "w": w})
    for al in (0.02, 0.04, 0.07, 0.1, 0.15, 0.2, 0.3, 0.5):
        pr, _ = variant(p, u.ewm_mean(alpha=al, adjust=False), f"a{al}")
        pv["alphas"].append({**score(f"{al}", pr), "alpha": al})
    al = 0.1
    pr, _ = variant(p, u.ewm_mean(alpha=al, adjust=False), "all"); pv["history"].append(score("all played games (EWMA 0.1)", pr))
    for mm in (5, 10, 20):
        pr, _ = variant(p.filter(pl.col("min") >= mm), u.ewm_mean(alpha=al, adjust=False), f"min{mm}")
        pv["history"].append(score(f"ignore games under {mm} min", pr))
    pr, _ = variant(p, u.clip(lo, hi).ewm_mean(alpha=al, adjust=False), "wins")
    pv["history"].append(score("winsorise usage at 1st/99th pct", pr))
    mn = pl.col("min")
    pr, _ = variant(p, (u * mn).ewm_mean(alpha=al, adjust=False) / mn.ewm_mean(alpha=al, adjust=False), "mw")
    pv["history"].append(score("minutes-weighted EWMA 0.1", pr))
    mw = np.where(np.isnan(pr), fall, pr)
    gk = np.concatenate([gid[n] for n in FN])
    gbm_all = np.concatenate([preds["LightGBM"][n] for n in FN])
    d_all = np.abs(gbm_all - yk) - np.abs(mw - yk)
    m = fk == FN[-1]
    out["paired_mw"] = {"pooled": list(cluster_boot(d_all, gk)), "holdout": list(cluster_boot(d_all[m], gk[m])),
                        "mw_mae": {n: float(np.abs(mw[fk == n] - yk[fk == n]).mean()) for n in FN},
                        "mw_metrics": {n: D.metrics(yk[fk == n], mw[fk == n]) for n in FN}}
    pr, _ = variant(p, u.ewm_mean(alpha=al, adjust=False, min_samples=10), "n10")
    pv["history"].append(score("EWMA 0.1, need 10 games else season/career", pr))
    out["processing"] = pv
    json.dump(out, open(D.CACHE / "analysis_a.json", "w"), default=float)
    print("done")


if __name__ == "__main__":
    main()

"""Follow-ups: (1) processed minutes-weighted EWMA features, (2) statyx tracking features (2024-25 -> 2025-26)."""
import json

import lightgbm as lgb
import numpy as np
import polars as pl

from . import data as D
from . import features as F
from .common import cluster_boot, lgb_over
from .gbm import PARAMS, prepared_folds

TRK = ["touches", "passes", "secondary_assists", "screen_assists", "contested_shots", "deflections", "box_outs",
       "speed", "distance"]


def processed_feats(box):
    p = F.player_history(box).select("player_id", "game_id", "game_date", "usg_pct", "min").sort(
        "player_id", "game_date", "game_id")
    u, mn = pl.col("usg_pct"), pl.col("min")
    q = p.with_columns(
        ((u * mn).ewm_mean(alpha=0.1, adjust=False).over("player_id")
         / mn.ewm_mean(alpha=0.1, adjust=False).over("player_id")).alias("usgw_ew1"),
        ((u * mn).ewm_mean(alpha=0.05, adjust=False).over("player_id")
         / mn.ewm_mean(alpha=0.05, adjust=False).over("player_id")).alias("usgw_ew05"))
    big = p.filter(mn >= 10).with_columns(u.ewm_mean(alpha=0.1, adjust=False).over("player_id").alias("usg_ew1_m10"))
    cols = ["usgw_ew1", "usgw_ew05"]
    q = q.with_columns([pl.col(c).shift(1).over("player_id") for c in cols]).select("player_id", "game_id", *cols)
    b = big.select("player_id", "game_date", "usg_ew1_m10").sort("game_date")
    return q, b


def run():
    df, a = D.load()
    box, _ = F.load_games()
    out = {}
    over = lgb_over()
    # ---- (1) processed features
    q, b = processed_feats(box)
    df = df.join(q, on=["player_id", "game_id"], how="left")
    df = (df.sort("game_date").join_asof(b, on="game_date", by="player_id", strategy="backward",
                                         allow_exact_matches=False))
    newf = ["usgw_ew1", "usgw_ew05", "usg_ew1_m10"]
    D.GROUPS["processed"] = newf
    folds = list(prepared_folds(df, a))
    base_f = D.cols(D.ORDER[:-0 or None] if False else [g for g in D.ORDER])
    res = {}
    P = {}
    for tag, feats in {"full": base_f, "full + processed": base_f + newf}.items():
        P[tag] = {}
        for n, tr, va in folds:
            m = lgb.LGBMRegressor(**{**PARAMS, **over}).fit(tr.select(feats).to_numpy(), tr["usg_pct"].to_numpy())
            P[tag][n] = m.predict(va.select(feats).to_numpy())
        res[tag] = {n: float(np.abs(P[tag][n] - va["usg_pct"].to_numpy()).mean())
                    for n, tr, va in folds}
        print(tag, res[tag], flush=True)
    yv = {n: va["usg_pct"].to_numpy() for n, tr, va in folds}
    gv = {n: va["game_id"].to_numpy() for n, tr, va in folds}
    d = {n: np.abs(P["full + processed"][n] - yv[n]) - np.abs(P["full"][n] - yv[n]) for n in yv}
    res["paired"] = {"pooled": list(cluster_boot(np.concatenate(list(d.values())),
                                                 np.concatenate(list(gv.values())))),
                     "holdout": list(cluster_boot(d[folds[-1][0]], gv[folds[-1][0]]))}
    out["processed"] = res

    # ---- (2) statyx tracking features, trained on 2024-25 only, tested on 2025-26
    adv = pl.read_parquet(D.D / "statyx/advanced_stats.parquet") if hasattr(D, "D") else pl.read_parquet(
        F.D / "statyx/advanced_stats.parquet")
    mp = pl.read_parquet(F.D / "util/player_id_map_vw.parquet").select("nba_id", "statyx_id").drop_nulls().unique("statyx_id")
    adv = adv.join(mp, left_on="player_id", right_on="statyx_id").select(
        pl.col("nba_id").alias("player_id"), "game_date", *TRK)
    bm = box.select("player_id", "game_date", "min").filter(pl.col("min") > 0).unique(["player_id", "game_date"])
    adv = adv.join(bm, on=["player_id", "game_date"], how="inner").sort("player_id", "game_date")
    rate = [(pl.col(c) / pl.col("min")).alias(f"trk_{c}") for c in TRK]
    adv = adv.with_columns(rate)
    tcols = [f"trk_{c}" for c in TRK]
    adv = adv.with_columns([pl.col(c).rolling_mean(10, min_samples=3).over("player_id").shift(1).over("player_id")
                            for c in tcols]).select("player_id", "game_date", *tcols)
    dd = df.join(adv, on=["player_id", "game_date"], how="left")
    feats0 = [c for c in base_f if c not in D.GROUPS["roles"]]
    tr = dd.filter(pl.col("season") == "2024-25")
    te = dd.filter(pl.col("season") == "2025-26")
    cover = float(te[tcols[0]].is_not_null().mean())
    r2 = {"train_rows": tr.height, "test_rows": te.height, "test_coverage": cover}
    Pp = {}
    for tag, feats in {"base features": feats0, "base + statyx tracking": feats0 + tcols}.items():
        m = lgb.LGBMRegressor(**{**PARAMS, "num_leaves": 15, "min_child_samples": 100}).fit(
            tr.select(feats).to_numpy(), tr["usg_pct"].to_numpy())
        Pp[tag] = m.predict(te.select(feats).to_numpy())
        r2[tag] = float(np.abs(Pp[tag] - te["usg_pct"].to_numpy()).mean())
    dpair = np.abs(Pp["base + statyx tracking"] - te["usg_pct"].to_numpy()) - np.abs(
        Pp["base features"] - te["usg_pct"].to_numpy())
    r2["paired"] = list(cluster_boot(dpair, te["game_id"].to_numpy()))
    out["statyx"] = r2
    print(r2, flush=True)
    json.dump(out, open(D.CACHE / "analysis_c.json", "w"), default=float)


if __name__ == "__main__":
    run()

import json
import numpy as np
import polars as pl
from . import data as D

PRED = {"season mean": "usg_season", "career mean": "usg_career", "last 1": "usg_l1", "last 3": "usg_l3",
        "last 5": "usg_l5", "last 10": "usg_l10", "last 20": "usg_l20", "minutes-wtd last 10": "usgw_l10",
        "EWMA a=0.1": "usg_ew1", "EWMA a=0.3": "usg_ew3"}


def run():
    df, a = D.load()
    out = {}
    for name, col in PRED.items():
        res = []
        for fold, tb, v in D.FOLDS:
            tr, va = D.split(df, tb, v)
            fb = tr["usg_pct"].mean()
            p = va.select(pl.coalesce(col, "usg_season", "usg_career", pl.lit(fb)).alias("p"))["p"].to_numpy()
            res.append({"fold": fold, **D.metrics(va["usg_pct"].to_numpy(), p)})
        out[name] = res
        print(f"{name:22s}", " ".join(f"{r['mae']:.4f}" for r in res))
    json.dump(out, open(D.CACHE / "baselines.json", "w"))


if __name__ == "__main__":
    run()

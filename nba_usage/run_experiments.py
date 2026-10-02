"""Run the baseline / model / ablation comparison and print a results table.

    uv run python -m nba_usage.run_experiments
"""

from __future__ import annotations

import argparse
from pathlib import Path

import polars as pl

from .dataset import build
from .evaluate import run_baselines, season_splits
from .features import feature_columns
from .injuries import ABSENCE_COLS, INJURY_START
from .models import fit_lightgbm, fit_ridge

CACHE = Path(".cache/usage_frame.parquet")


def main() -> None:
    ap = argparse.ArgumentParser()
    ap.add_argument("--folds", type=int, default=2)
    ap.add_argument("--rebuild", action="store_true")
    ap.add_argument("--all-seasons", action="store_true", help="ignore the injury-era restriction")
    args = ap.parse_args()

    frame = build(cache=CACHE, rebuild=args.rebuild)
    if not args.all_seasons:
        frame = frame.filter(pl.col("game_date") >= pl.lit(INJURY_START).str.to_date())

    base_cols = [c for c in feature_columns(frame) if c not in ABSENCE_COLS]
    full_cols = base_cols + [c for c in ABSENCE_COLS if c in frame.columns]

    rows = []
    for sp in season_splits(sorted(frame["season"].unique().to_list()), n_folds=args.folds, min_train=2):
        train = frame.filter(pl.col("season").is_in(sp.train))
        valid = frame.filter(pl.col("season").is_in(sp.valid))

        for r in run_baselines(train, valid).iter_rows(named=True):
            rows.append({"fold": sp.name, "features": "-", **r})
        for name, fit in (("ridge", fit_ridge), ("lightgbm", fit_lightgbm)):
            for label, cols in (("history", base_cols), ("history+absence", full_cols)):
                m, _ = fit(train, valid, cols)
                rows.append({"fold": sp.name, "features": label, "model": name, **m})

    results = pl.DataFrame(rows).select("fold", "model", "features", "mae", "rmse", "r2", "n")
    with pl.Config(tbl_rows=60, float_precision=5):
        print(results.sort("fold", "mae"))


if __name__ == "__main__":
    main()

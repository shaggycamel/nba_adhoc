"""Final comparison: every algorithm on the recommended feature set, same folds.

    uv run python -m nba_usage.run_final

Writes RESULTS.md so the numbers in the write-up can be regenerated.
"""

from __future__ import annotations

from pathlib import Path

import polars as pl

from .dataset import build
from .evaluate import run_baselines, season_splits
from .features import feature_columns
from .hierarchy import ROTATION_COLS
from .injuries import ABSENCE_COLS, INJURY_START
from .models import fit_catboost, fit_elasticnet, fit_forest, fit_lightgbm, fit_ridge, fit_xgboost
from .roles import N_ROLES
from .synergy import ABSORPTION_COLS

CACHE = Path(".cache/usage_frame.parquet")
OUT = Path("RESULTS.md")

# Features the ablations showed are worth keeping. Role clusters and pairwise
# absorption are excluded: both failed to beat the set without them.
EXCLUDED = set(ABSORPTION_COLS) | {f"role_{r}" for r in range(N_ROLES)} | {
    "role",
    "vacated_same_role",
    "vacated_other_role",
}

ALGORITHMS = {
    "ridge": fit_ridge,
    "elasticnet": fit_elasticnet,
    "lightgbm": fit_lightgbm,
    "xgboost": fit_xgboost,
    "catboost": fit_catboost,
    "random_forest": lambda a, b, c: fit_forest(a, b, c, kind="rf"),
    "extra_trees": lambda a, b, c: fit_forest(a, b, c, kind="et"),
}


def recommended_columns(frame: pl.DataFrame) -> list[str]:
    extra = [c for c in ABSENCE_COLS + ROTATION_COLS if c in frame.columns]
    cols = [c for c in feature_columns(frame) + extra if c not in EXCLUDED]
    return list(dict.fromkeys(cols))


def main() -> None:
    frame = build(cache=CACHE).filter(pl.col("game_date") >= pl.lit(INJURY_START).str.to_date())
    cols = recommended_columns(frame)
    rows = []

    for sp in season_splits(sorted(frame["season"].unique().to_list()), n_folds=2, min_train=2):
        train = frame.filter(pl.col("season").is_in(sp.train))
        valid = frame.filter(pl.col("season").is_in(sp.valid))
        for r in run_baselines(train, valid).iter_rows(named=True):
            rows.append({"fold": sp.name, "kind": "baseline", **r})
        for name, fit in ALGORITHMS.items():
            m, _ = fit(train, valid, cols)
            rows.append({"fold": sp.name, "kind": "model", "model": name, **m})

    results = pl.DataFrame(rows).select("fold", "kind", "model", "mae", "rmse", "r2", "n")
    with pl.Config(tbl_rows=60, float_precision=5):
        print(results.sort("fold", "mae"))

    lines = [
        "# Results",
        "",
        f"Target `usg_pct`, {len(cols)} features, injury-era seasons only "
        "(2021-22 onward, the first season the injury report covers).",
        "Expanding-window season folds; no shuffling anywhere.",
        "",
        "| fold | kind | model | MAE | RMSE | R2 | n |",
        "| --- | --- | --- | --- | --- | --- | --- |",
    ]
    for r in results.sort(["fold", "mae"]).iter_rows(named=True):
        lines.append(
            f"| {r['fold']} | {r['kind']} | {r['model']} | {r['mae']:.5f} | "
            f"{r['rmse']:.5f} | {r['r2']:.4f} | {r['n']} |"
        )
    OUT.write_text("\n".join(lines) + "\n")
    print(f"\nwrote {OUT}")


if __name__ == "__main__":
    main()

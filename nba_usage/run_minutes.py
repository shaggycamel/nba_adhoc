"""Does knowing how long an absence will last help predict minutes?

    uv run python -m nba_usage.run_minutes

The injury report says who is out tonight, so a survival model adds nothing
to that. What it can add is severity: a rested veteran and a torn calf are
both "Out", and a coach fills those gaps differently. This ablates the
spell block (expected remaining absence from the hazard model, how many
games the team has already played short-handed, and how recently the focal
player returned from their own absence) on the minutes target, then checks
whether any gain carries through to points.
"""

from __future__ import annotations

from pathlib import Path

import numpy as np
import polars as pl

from .attributes import ATTRIBUTE_COLS
from .context import CONTEXT_COLS
from .evaluate import metrics, season_splits
from .hierarchy import ROTATION_COLS
from .injuries import ABSENCE_COLS, INJURY_START
from .minutes import MINUTES_COLS
from .models import design_matrix, fit_lightgbm, fit_ridge
from .spells import SPELL_COLS
from .standings import STANDINGS_COLS
from .volume import volume_feature_columns

OUT = Path("RESULTS_minutes.md")
FRAME = Path(".cache/volume_frame4.parquet")

SLICES = {
    "all rows": pl.lit(True),
    "starter out": pl.col("vacated_starter_min") >= 1,
    "starter out, player is a backup": (pl.col("vacated_starter_min") >= 1) & (pl.col("min_r10") < 24),
    "first game of the absence": pl.col("absent_fresh") >= 1,
}


def main() -> None:
    frame = pl.read_parquet(FRAME).filter(pl.col("game_date") >= pl.lit(INJURY_START).str.to_date())
    base = volume_feature_columns(frame, extra=ABSENCE_COLS + ROTATION_COLS)
    enriched = base + [c for c in CONTEXT_COLS + MINUTES_COLS if c in frame.columns]
    with_spells = enriched + [c for c in SPELL_COLS if c in frame.columns]
    with_attrs = with_spells + [c for c in ATTRIBUTE_COLS if c in frame.columns]
    with_stand = with_spells + [c for c in STANDINGS_COLS if c in frame.columns]
    everything = with_attrs + [c for c in STANDINGS_COLS if c in frame.columns]
    for cl in (with_spells, with_attrs, with_stand, everything):
        assert "min" not in cl and "pts" not in cl

    rows: list[dict] = []
    for sp in season_splits(sorted(frame["season"].unique().to_list()), n_folds=2, min_train=2):
        train = frame.filter(pl.col("season").is_in(sp.train))
        valid = frame.filter(pl.col("season").is_in(sp.valid))
        masks = {n: valid.select(e.alias("m"))["m"].fill_null(False).to_numpy() for n, e in SLICES.items()}

        def record(target, method, y, pred):
            for sname, m in masks.items():
                if m.sum() >= 200:
                    rows.append({"fold": sp.name, "target": target, "slice": sname,
                                 "method": method, **metrics(y[m], pred[m])})

        y_min = valid["min"].to_numpy()
        fb = float(train["min"].mean())
        record("min", "baseline last10", y_min, valid["min_r10"].fill_null(fb).fill_nan(fb).to_numpy())

        minutes_preds = {}
        for fname, fit in (("ridge", fit_ridge), ("lightgbm", fit_lightgbm)):
            for cname, cols in (
                ("no spells", enriched),
                ("+spells", with_spells),
                ("+attributes", with_attrs),
                ("+standings", with_stand),
                ("+attrs+standings", everything),
            ):
                _, model = fit(train, valid, cols, target="min")
                p = np.clip(np.asarray(model.predict(design_matrix(valid, cols))), 0.0, float(train["min"].max()))
                minutes_preds[(fname, cname)] = (p, cols)
                record("min", f"{fname} {cname}", y_min, p)

        # Does any minutes gain carry through to a points projection?
        y_pts = valid["pts"].to_numpy()
        for (fname, cname), (mp, cols) in minutes_preds.items():
            rate_cols = cols + ["usg_hat"]
            fit = fit_ridge if fname == "ridge" else fit_lightgbm
            _, rm = fit(train.drop_nulls("pts_pm"), valid, rate_cols, target="pts_pm")
            rate = np.clip(np.asarray(rm.predict(design_matrix(valid, rate_cols))), 0.0, None)
            record("pts", f"{fname} pipeline {cname}", y_pts, np.clip(mp * rate, 0.0, None))

    res = pl.DataFrame(rows).select("fold", "target", "slice", "method", "mae", "rmse", "r2", "n")
    with pl.Config(tbl_rows=120, float_precision=4):
        print(res.sort(["target", "slice", "fold", "mae"]))

    lines = [
        "# Minutes and absence duration",
        "",
        "`+spells` adds the hazard model's expected remaining absence for the",
        "team's absentees, how many games the team has already played short-handed,",
        "and how recently the focal player returned from an absence of their own.",
        "",
        "| fold | target | slice | method | mae | rmse | r2 | n |",
        "|---|---|---|---|---|---|---|---|",
    ]
    for r in res.sort(["target", "slice", "fold", "mae"]).iter_rows(named=True):
        lines.append(f"| {r['fold']} | {r['target']} | {r['slice']} | {r['method']} | "
                     f"{r['mae']:.4f} | {r['rmse']:.4f} | {r['r2']:.4f} | {r['n']} |")
    OUT.write_text("\n".join(lines) + "\n")
    print(f"\nwrote {OUT}")


if __name__ == "__main__":
    main()

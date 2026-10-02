"""Volume projection: minutes model, opponent context, and a non-linear rate model.

    uv run python -m nba_usage.run_volume

Three questions at once, on the same folds:

  1. Can a dedicated minutes model beat a recent-minutes average, especially
     when a starter is out? Minutes is the factor injury news determines.
  2. Do opponent and own-team context features help volume, given they were
     never needed for usage?
  3. Does feeding usage into a *non-linear* rate model help, where feeding it
     into a linear one provably cannot?

Each model is fitted once per fold and predicts the whole validation season;
the slices are taken from those predictions, so every slice sees the same
model rather than one refitted on it.
"""

from __future__ import annotations

from pathlib import Path

import numpy as np
import polars as pl

from .context import CONTEXT_COLS, add_context_features
from .evaluate import metrics, season_splits
from .hierarchy import ROTATION_COLS, add_rotation_features
from .injuries import ABSENCE_COLS, INJURY_START, absence_features
from .minutes import MINUTES_COLS, add_allocation_features, add_responsiveness
from .models import design_matrix, fit_lightgbm, fit_ridge
from .panel import add_game_order, load_panel
from .volume import (
    VOLUME_STATS,
    add_volume_features,
    add_volume_targets,
    oof_usage,
    volume_feature_columns,
)

OUT = Path("RESULTS_volume.md")
CACHE = Path(".cache/volume_frame2.parquet")

SLICES = {
    "all rows": pl.lit(True),
    "starter out": pl.col("vacated_starter_min") >= 1,
    "starter out, player is a backup": (pl.col("vacated_starter_min") >= 1) & (pl.col("min_r10") < 24),
}


def build_frame(rebuild: bool = False) -> pl.DataFrame:
    if CACHE.exists() and not rebuild:
        return pl.read_parquet(CACHE)
    panel = add_game_order(load_panel())
    played = panel.filter(pl.col("played"))
    frame = (
        add_volume_features(played)
        .join(absence_features(panel, played), on=["game_id", "player_id", "team_abbreviation"], how="left")
        .join(add_rotation_features(panel, played), on=["game_id", "player_id", "team_abbreviation"], how="left")
        .with_columns(rotation_rank=pl.col("rotation_rank").fill_null(pl.col("rotation_size")))
    )
    frame = add_volume_targets(frame)
    frame = add_responsiveness(add_allocation_features(add_context_features(frame)))
    base = volume_feature_columns(frame, extra=ABSENCE_COLS + ROTATION_COLS)
    frame = oof_usage(frame, base, fit_ridge)
    CACHE.parent.mkdir(parents=True, exist_ok=True)
    frame.write_parquet(CACHE)
    return frame


def _predict(fit, train: pl.DataFrame, valid: pl.DataFrame, cols: list[str], target: str) -> np.ndarray:
    _, model = fit(train.drop_nulls(target), valid, cols, target=target)
    return np.asarray(model.predict(design_matrix(valid, cols)))


def _slice_masks(valid: pl.DataFrame) -> dict[str, np.ndarray]:
    out = {}
    for name, expr in SLICES.items():
        out[name] = valid.select(expr.alias("m"))["m"].fill_null(False).to_numpy()
    return out


def main() -> None:
    frame = build_frame().filter(pl.col("game_date") >= pl.lit(INJURY_START).str.to_date())

    base_cols = volume_feature_columns(frame, extra=ABSENCE_COLS + ROTATION_COLS)
    enriched = base_cols + [c for c in CONTEXT_COLS + MINUTES_COLS if c in frame.columns]
    banned = set(VOLUME_STATS) | {f"{s}_pm" for s in VOLUME_STATS} | {"min", "usg_pct"}
    assert not banned & set(enriched), f"leak: {banned & set(enriched)}"

    rows: list[dict] = []

    def record(fold, target, slice_name, method, y, pred, mask):
        if mask.sum() < 200:
            return
        rows.append({"fold": fold, "target": target, "slice": slice_name, "method": method,
                     **metrics(y[mask], pred[mask])})

    for sp in season_splits(sorted(frame["season"].unique().to_list()), n_folds=2, min_train=2):
        train = frame.filter(pl.col("season").is_in(sp.train)).drop_nulls("usg_hat")
        valid = frame.filter(pl.col("season").is_in(sp.valid))
        masks = _slice_masks(valid)

        # --- minutes, against a recent-minutes average -------------------
        y_min = valid["min"].to_numpy()
        for bname, col in (("baseline last5", "min_r5"), ("baseline last10", "min_r10")):
            fb = float(train["min"].mean())
            pred = valid[col].fill_null(fb).fill_nan(fb).to_numpy()
            for sname, m in masks.items():
                record(sp.name, "min", sname, bname, y_min, pred, m)
        # The structural allocation on its own, with no model at all.
        alloc = valid["expected_min_alloc"].fill_null(float(train["min"].mean())).to_numpy()
        for sname, m in masks.items():
            record(sp.name, "min", sname, "expected_min_alloc (no model)", y_min, alloc, m)

        minutes_preds: dict[tuple[str, str], np.ndarray] = {}
        for fname, fit in (("ridge", fit_ridge), ("lightgbm", fit_lightgbm)):
            for cname, cols in (("base", base_cols), ("+context+alloc", enriched)):
                p = np.clip(_predict(fit, train, valid, cols, "min"), 0.0, float(train["min"].max()))
                minutes_preds[(fname, cname)] = p
                for sname, m in masks.items():
                    record(sp.name, "min", sname, f"{fname} {cname}", y_min, p, m)

        # --- volume stats -------------------------------------------------
        for stat in VOLUME_STATS:
            y = valid[stat].to_numpy()
            fb = float(train[stat].mean())
            for bname, col in ((f"baseline last10", f"{stat}_r10"),):
                pred = valid[col].fill_null(fb).fill_nan(fb).to_numpy()
                for sname, m in masks.items():
                    record(sp.name, stat, sname, bname, y, pred, m)

            for fname, fit in (("ridge", fit_ridge), ("lightgbm", fit_lightgbm)):
                for cname, cols in (("base", base_cols), ("+context+alloc", enriched)):
                    direct = np.clip(_predict(fit, train, valid, cols, stat), 0.0, None)
                    for sname, m in masks.items():
                        record(sp.name, stat, sname, f"{fname} direct {cname}", y, direct, m)

                    rate_cols = cols + ["usg_hat"]
                    rate = np.clip(_predict(fit, train, valid, rate_cols, f"{stat}_pm"), 0.0, None)
                    pipe = np.clip(minutes_preds[(fname, cname)] * rate, 0.0, None)
                    for sname, m in masks.items():
                        record(sp.name, stat, sname, f"{fname} pipeline {cname}", y, pipe, m)

    res = pl.DataFrame(rows).select("fold", "target", "slice", "method", "mae", "rmse", "r2", "n")
    with pl.Config(tbl_rows=200, float_precision=4):
        print(res.sort(["target", "slice", "fold", "mae"]))

    lines = [
        "# Volume projection",
        "",
        "Minutes, points, rebounds and assists on the usage project's folds.",
        "Each model is fitted once per fold and predicts the whole validation",
        "season; slices are taken from those predictions. The usage prediction",
        "fed to the rate models (`usg_hat`) is out-of-fold.",
        "",
        "`+context+alloc` adds opponent and own-team rolling form, the minutes",
        "allocation implied by the healthy rotation, and each player's measured",
        "minutes response to a starter sitting.",
        "",
        "| fold | target | slice | method | mae | rmse | r2 | n |",
        "|---|---|---|---|---|---|---|---|",
    ]
    for r in res.sort(["target", "slice", "fold", "mae"]).iter_rows(named=True):
        lines.append(
            f"| {r['fold']} | {r['target']} | {r['slice']} | {r['method']} | "
            f"{r['mae']:.4f} | {r['rmse']:.4f} | {r['r2']:.4f} | {r['n']} |"
        )
    OUT.write_text("\n".join(lines) + "\n")
    print(f"\nwrote {OUT}")


if __name__ == "__main__":
    main()

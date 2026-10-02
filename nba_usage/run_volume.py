"""Project points, rebounds and assists, and check the stepping-up case.

    uv run python -m nba_usage.run_volume

Compares three ways of getting a volume projection, on the same folds:

    baseline   the player's recent average of the stat
    direct     one model straight onto the stat total
    pipeline   predicted minutes x predicted per-minute rate, the rate model
               also given an out-of-fold usage prediction

and reports each on the whole validation season and on two slices: games
where a starter-minutes teammate is ruled out, and the subset of those where
the player is not a starter themselves. That second slice is the case the
pipeline exists for.
"""

from __future__ import annotations

from pathlib import Path

import polars as pl

from .evaluate import season_splits
from .hierarchy import ROTATION_COLS, add_rotation_features
from .injuries import ABSENCE_COLS, INJURY_START, absence_features
from .models import fit_ridge
from .panel import add_game_order, load_panel
from .volume import (
    VOLUME_STATS,
    add_volume_features,
    add_volume_targets,
    compose_volume,
    fit_direct_volume,
    fit_minutes,
    oof_usage,
    volume_baselines,
    volume_feature_columns,
)

OUT = Path("RESULTS_volume.md")
CACHE = Path(".cache/volume_frame.parquet")


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
    cols = volume_feature_columns(frame, extra=ABSENCE_COLS + ROTATION_COLS)
    frame = oof_usage(frame, cols, fit_ridge)
    CACHE.parent.mkdir(parents=True, exist_ok=True)
    frame.write_parquet(CACHE)
    return frame


def main() -> None:
    frame = build_frame().filter(pl.col("game_date") >= pl.lit(INJURY_START).str.to_date())
    base_cols = volume_feature_columns(frame, extra=ABSENCE_COLS + ROTATION_COLS)
    rate_cols = base_cols + ["usg_hat"]
    for leak in VOLUME_STATS + [f"{s}_pm" for s in VOLUME_STATS] + ["min", "usg_pct"]:
        assert leak not in rate_cols, f"{leak} leaked into the features"

    # The slices that matter: a starter-minutes teammate ruled out, and the
    # subset where the player was not already a starter.
    slices = {
        "all rows": pl.lit(True),
        "starter out": pl.col("vacated_starter_min") >= 1,
        "starter out, player is a backup": (pl.col("vacated_starter_min") >= 1) & (pl.col("min_r10") < 24),
    }

    rows = []
    for sp in season_splits(sorted(frame["season"].unique().to_list()), n_folds=2, min_train=2):
        train = frame.filter(pl.col("season").is_in(sp.train)).drop_nulls("usg_hat")
        valid = frame.filter(pl.col("season").is_in(sp.valid))
        minutes_pred_cache: dict[int, object] = {}

        for stat in VOLUME_STATS:
            for sname, expr in slices.items():
                mask = valid.with_row_index("_i").filter(expr)
                idx = mask["_i"].to_numpy()
                sub = mask.drop("_i")
                if sub.height < 200:
                    continue
                for bname, m in volume_baselines(train, sub, stat).items():
                    rows.append({"fold": sp.name, "stat": stat, "slice": sname, "method": f"baseline {bname}", **m})
                d = fit_direct_volume(fit_ridge, train, sub, base_cols, stat)
                rows.append({"fold": sp.name, "stat": stat, "slice": sname, "method": "direct", **d})
                if 0 not in minutes_pred_cache:
                    minutes_pred_cache[0] = fit_minutes(fit_ridge, train, valid, base_cols)
                mp = minutes_pred_cache[0][idx]
                p, _ = compose_volume(fit_ridge, train, sub, rate_cols, stat, minutes_pred=mp)
                rows.append({"fold": sp.name, "stat": stat, "slice": sname, "method": "pipeline (min x rate, usage fed)", **p})

    res = pl.DataFrame(rows).select("fold", "stat", "slice", "method", "mae", "rmse", "r2", "n")
    with pl.Config(tbl_rows=120, float_precision=4):
        print(res.sort(["stat", "slice", "fold", "mae"]))

    lines = [
        "# Volume projection",
        "",
        "Points, rebounds and assists. Same folds and discipline as the usage work;",
        "the usage prediction fed to the rate models is out-of-fold.",
        "",
        "| fold | stat | slice | method | mae | rmse | r2 | n |",
        "|---|---|---|---|---|---|---|---|",
    ]
    for r in res.sort(["stat", "slice", "fold", "mae"]).iter_rows(named=True):
        lines.append(
            f"| {r['fold']} | {r['stat']} | {r['slice']} | {r['method']} | "
            f"{r['mae']:.4f} | {r['rmse']:.4f} | {r['r2']:.4f} | {r['n']} |"
        )
    OUT.write_text("\n".join(lines) + "\n")
    print(f"\nwrote {OUT}")


if __name__ == "__main__":
    main()

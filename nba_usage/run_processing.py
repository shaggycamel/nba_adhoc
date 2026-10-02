"""Processing-choice experiments: window lengths, weighting, DNP handling,
outlier treatment, injury encodings, and the smallest feature set that holds up.

    uv run python -m nba_usage.run_processing

Every variant is scored on the same folds with the same model (Ridge, which
was at least as good as the boosters on the full feature set and is fast
enough to sweep), so differences are down to the processing choice.
"""

from __future__ import annotations

from pathlib import Path

import polars as pl

from .evaluate import metrics, season_splits
from .features import add_prior_features, feature_columns
from .hierarchy import ROTATION_COLS, add_rotation_features
from .injuries import ABSENCE_COLS, INJURY_START, absence_features
from .models import fit_ridge
from .panel import add_game_order, load_panel

FOLDS = 2


def _frame(windows, halflives, panel, played) -> pl.DataFrame:
    feat = add_prior_features(played, windows=windows, halflives=halflives)
    return (
        feat.join(absence_features(panel, played), on=["game_id", "player_id", "team_abbreviation"], how="left")
        .join(add_rotation_features(panel, played), on=["game_id", "player_id", "team_abbreviation"], how="left")
        .with_columns(rotation_rank=pl.col("rotation_rank").fill_null(pl.col("rotation_size")))
        .filter(pl.col("game_date") >= pl.lit(INJURY_START).str.to_date())
    )


def _score(frame: pl.DataFrame, cols: list[str]) -> dict[str, float]:
    """Mean MAE across folds, so variants are compared on one number."""
    out = []
    for sp in season_splits(sorted(frame["season"].unique().to_list()), n_folds=FOLDS, min_train=2):
        tr = frame.filter(pl.col("season").is_in(sp.train))
        va = frame.filter(pl.col("season").is_in(sp.valid))
        m, _ = fit_ridge(tr, va, cols)
        out.append(m)
    return {
        "mae": sum(m["mae"] for m in out) / len(out),
        "r2": sum(m["r2"] for m in out) / len(out),
    }


def _cols(frame: pl.DataFrame) -> list[str]:
    extra = [c for c in ABSENCE_COLS + ROTATION_COLS if c in frame.columns]
    return list(dict.fromkeys(feature_columns(frame) + extra))


def windows_and_weighting(panel, played) -> pl.DataFrame:
    variants = {
        "short (3,5)": ((3, 5), (3.0,)),
        "medium (3,5,10)": ((3, 5, 10), (3.0, 10.0)),
        "default (3,5,10,20)": ((3, 5, 10, 20), (3.0, 10.0)),
        "long (5,10,20,40)": ((5, 10, 20, 40), (3.0, 10.0, 20.0)),
        "ewma only": ((10,), (1.0, 3.0, 10.0, 20.0)),
    }
    rows = []
    for name, (w, hl) in variants.items():
        frame = _frame(w, hl, panel, played)
        rows.append({"variant": name, "n_features": len(_cols(frame)), **_score(frame, _cols(frame))})
    return pl.DataFrame(rows)


def injury_encodings(frame: pl.DataFrame) -> pl.DataFrame:
    base = [c for c in feature_columns(frame) if c not in set(ABSENCE_COLS) | set(ROTATION_COLS)]
    variants = {
        "none": [],
        "count only": ["n_teammates_out"],
        "load only": ["vacated_usg_min"],
        "share only": ["vacated_share"],
        "own status only": ["own_status_rank"],
        "all absence": [c for c in ABSENCE_COLS if c in frame.columns],
        "all absence + rotation": [c for c in ABSENCE_COLS + ROTATION_COLS if c in frame.columns],
    }
    rows = []
    for name, extra in variants.items():
        cols = list(dict.fromkeys(base + extra))
        rows.append({"encoding": name, "n_features": len(cols), **_score(frame, cols)})
    return pl.DataFrame(rows)


def outlier_and_dnp(frame: pl.DataFrame) -> pl.DataFrame:
    """Does trimming junk rows help? Garbage-time and cameo appearances have
    wild usage and may be teaching the model noise."""
    cols = _cols(frame)
    rows = []
    for name, f in {
        "all played rows": frame,
        "min >= 5": frame.filter(pl.col("min") >= 5),
        "min >= 10": frame.filter(pl.col("min") >= 10),
        "usage winsorised 1-99%": frame.with_columns(
            usg_pct=pl.col("usg_pct").clip(
                frame["usg_pct"].quantile(0.01), frame["usg_pct"].quantile(0.99)
            )
        ),
        ">= 5 prior games": frame.filter(pl.col("season_game_n") >= 5),
    }.items():
        rows.append({"treatment": name, "n_rows": f.height, **_score(f, cols)})
    return pl.DataFrame(rows)


def greedy_subset(frame: pl.DataFrame, max_features: int = 12) -> pl.DataFrame:
    """Smallest set that forecasts well: add the feature that helps most, stop
    when nothing adds more than a hair."""
    pool = _cols(frame)
    chosen: list[str] = []
    rows = []
    seasons = sorted(frame["season"].unique().to_list())
    sp = season_splits(seasons, n_folds=1, min_train=2)[0]
    tr = frame.filter(pl.col("season").is_in(sp.train))
    va = frame.filter(pl.col("season").is_in(sp.valid))

    best_so_far = float("inf")
    for _ in range(max_features):
        scored = []
        for c in pool:
            if c in chosen:
                continue
            m, _ = fit_ridge(tr, va, chosen + [c])
            scored.append((m["mae"], m["r2"], c))
        scored.sort()
        mae, r2, col = scored[0]
        chosen.append(col)
        rows.append({"n": len(chosen), "added": col, "mae": mae, "r2": r2})
        if best_so_far - mae < 1e-5:
            break
        best_so_far = mae
    return pl.DataFrame(rows)


def _md(title: str, df: pl.DataFrame) -> str:
    head = f"## {title}\n\n| " + " | ".join(df.columns) + " |\n|" + "---|" * len(df.columns)
    body = [
        "| " + " | ".join(f"{v:.5f}" if isinstance(v, float) else str(v) for v in row) + " |"
        for row in df.iter_rows()
    ]
    return "\n".join([head, *body, ""])


def main() -> None:
    panel = add_game_order(load_panel())
    played = panel.filter(pl.col("played"))
    default = _frame((3, 5, 10, 20), (3.0, 10.0), panel, played)

    tables = {
        "Window length and weighting (Ridge, mean over folds)": windows_and_weighting(panel, played).sort("mae"),
        "Injury encodings (Ridge, mean over folds)": injury_encodings(default).sort("mae"),
        "Outlier and DNP handling (row sets differ, so MAE is not comparable across rows)": outlier_and_dnp(default),
        "Greedy forward selection (Ridge, validated on the last season)": greedy_subset(default),
    }
    with pl.Config(tbl_rows=40, float_precision=5):
        for title, df in tables.items():
            print(f"\n== {title} ==")
            print(df)

    out = Path("RESULTS_processing.md")
    out.write_text(
        "# Processing experiments\n\nAll variants scored with Ridge on the same "
        "expanding-window season folds.\n\n"
        + "\n".join(_md(t, d) for t, d in tables.items())
    )
    print(f"\nwrote {out}")


if __name__ == "__main__":
    main()

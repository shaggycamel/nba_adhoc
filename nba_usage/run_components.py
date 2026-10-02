"""Compare predicting usage directly against composing it from its components.

    uv run python -m nba_usage.run_components

Writes RESULTS_components.md. All three approaches use the same 97 features
and the same folds, so the only thing varying is how the target is
parameterised.
"""

from __future__ import annotations

from pathlib import Path

import polars as pl

from .components import component_targets, compose_components, compose_rate_ratio, verify_identity
from .dataset import build
from .evaluate import run_baselines, season_splits
from .injuries import INJURY_START
from .models import fit_lightgbm, fit_ridge
from .run_final import recommended_columns

# Same-game quantities: targets for the component models, never inputs.
COMPONENT_TARGETS = {"fga_pm", "fta_pm", "tov_pm", "player_rate", "team_rate", "usg_recon"}
OUT = Path("RESULTS_components.md")


def main() -> None:
    frame = build(cache=Path(".cache/usage_frame.parquet")).join(
        component_targets(), on=["game_id", "player_id"], how="left"
    )
    frame = frame.filter(pl.col("game_date") >= pl.lit(INJURY_START).str.to_date())
    cols = [c for c in recommended_columns(frame) if c not in COMPONENT_TARGETS]
    assert not set(cols) & COMPONENT_TARGETS, "component targets leaked into the features"

    floor = verify_identity(frame.drop_nulls("usg_recon"))
    rows = []
    for sp in season_splits(sorted(frame["season"].unique().to_list()), n_folds=2, min_train=2):
        train = frame.filter(pl.col("season").is_in(sp.train))
        valid = frame.filter(pl.col("season").is_in(sp.valid))
        base = run_baselines(train, valid).sort("mae").row(0, named=True)
        rows.append({"fold": sp.name, "model": f"baseline ({base['model']})", "approach": "-", **{k: base[k] for k in ("mae", "r2")}})
        for name, fit in (("ridge", fit_ridge), ("lightgbm", fit_lightgbm)):
            direct, _ = fit(train, valid, cols)
            rows.append({"fold": sp.name, "model": name, "approach": "direct", "mae": direct["mae"], "r2": direct["r2"]})
            for label, compose in (("rate ratio", compose_rate_ratio), ("components", compose_components)):
                m = compose(fit, train, valid, cols)
                rows.append({"fold": sp.name, "model": name, "approach": label, "mae": m["mae"], "r2": m["r2"]})

    results = pl.DataFrame(rows)
    with pl.Config(tbl_rows=40, float_precision=5):
        print(results)

    lines = [
        "# Component decomposition",
        "",
        "Usage rearranges to the player's event rate per minute divided by the team's",
        "event rate per game-minute, so it can be composed from predicted parts instead",
        "of predicted directly. Same features, same folds, three parameterisations.",
        "",
        f"Reconstructing `usg_pct` from the **actual observed** components misses the stored",
        f"column by {floor['mae']:.5f} MAE (median {floor['median']:.5f}, correlation {floor['corr']:.4f}).",
        "Minutes are whole numbers in this dataset and usage is stored to three decimals,",
        "so that is the floor any composed approach inherits before a model is fitted.",
        "",
        "| fold | model | approach | mae | r2 |",
        "|---|---|---|---|---|",
    ]
    for r in results.iter_rows(named=True):
        lines.append(f"| {r['fold']} | {r['model']} | {r['approach']} | {r['mae']:.5f} | {r['r2']:.5f} |")
    OUT.write_text("\n".join(lines) + "\n")
    print(f"\nwrote {OUT}")


if __name__ == "__main__":
    main()

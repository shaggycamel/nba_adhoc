"""Evaluate the layer 2 slot model against baselines on held-out seasons.

Walk-forward: for each validation season, train only on seasons that finished
before it. Accuracy is measured on rows where a player actually started, which
is the only place `start_position` gives a label.
"""

from __future__ import annotations

import numpy as np
import polars as pl

from nba_hierarchy.data import load_player_games
from nba_hierarchy.position import (
    HISTORY_FEATURES,
    PROFILE_FEATURES,
    SLOTS,
    constrained_slot_assignment,
    fit_slot_model,
    predict_slots,
    prepare,
)
from nba_hierarchy.roster import build_panel
from nba_hierarchy.state import add_pre_game_state

VALIDATION_SEASONS = ["2023-24", "2024-25", "2025-26"]


def modal_slot_baseline(df: pl.DataFrame) -> pl.Series:
    """Predict the slot a player has started at most often before this game."""
    counts = np.column_stack([df[f"prior_starts_{s}"].to_numpy() for s in SLOTS])
    best = counts.argmax(axis=1)
    has_history = counts.sum(axis=1) > 0
    return pl.Series(
        [SLOTS[i] if h else None for i, h in zip(best, has_history)]
    )


def accuracy(pred: pl.Series, truth: pl.Series) -> float:
    mask = pred.is_not_null()
    if mask.sum() == 0:
        return float("nan")
    return float((pred.filter(mask) == truth.filter(mask)).mean())


def main() -> None:
    state = prepare(add_pre_game_state(build_panel(pg=load_player_games())))
    starters = state.filter(pl.col("start_position").is_in(SLOTS))
    print(f"labelled starter rows: {starters.height:,}\n")

    combined = PROFILE_FEATURES + HISTORY_FEATURES
    rows = []
    for season in VALIDATION_SEASONS:
        train = starters.filter(pl.col("season") < season)
        test = starters.filter(pl.col("season") == season)
        if not train.height or not test.height:
            continue

        truth = test["start_position"]
        majority = train["start_position"].mode().first()
        modal = modal_slot_baseline(test)

        m_profile = fit_slot_model(train, PROFILE_FEATURES)
        m_combined = fit_slot_model(train, combined)
        scored_profile = predict_slots(m_profile, test, PROFILE_FEATURES)
        scored_combined = predict_slots(m_combined, test, combined)

        # Constrained assignment reorders rows, so compare against its own
        # truth column rather than the original ordering.
        con_profile = constrained_slot_assignment(scored_profile)
        con_combined = constrained_slot_assignment(scored_combined)

        # Baseline with a majority fallback where a player has no prior starts,
        # so every method is scored on the same rows.
        modal_filled = pl.Series(
            [m if m is not None else majority for m in modal]
        )

        rows.append(
            {
                "season": season,
                "test_rows": test.height,
                "majority": accuracy(pl.Series([majority] * test.height), truth),
                "modal_prior": accuracy(modal_filled, truth),
                "modal_coverage": float(modal.is_not_null().mean()),
                "profile": accuracy(scored_profile["slot"], truth),
                "profile+history": accuracy(scored_combined["slot"], truth),
                "constrained_profile": accuracy(
                    con_profile["slot"], con_profile["start_position"]
                ),
                "constrained_combined": accuracy(
                    con_combined["slot"], con_combined["start_position"]
                ),
            }
        )

    res = pl.DataFrame(rows)
    pl.Config.set_tbl_width_chars(140)
    print(res.select(
        "season", "test_rows",
        pl.col("majority").round(4),
        pl.col("modal_prior").round(4),
        pl.col("profile").round(4),
        pl.col("profile+history").round(4),
        pl.col("constrained_profile").round(4),
        pl.col("constrained_combined").round(4),
    ))
    print("\nmean over validation seasons:")
    for c in ("majority", "modal_prior", "profile", "profile+history",
              "constrained_profile", "constrained_combined"):
        print(f"  {c:>16}: {res[c].mean():.4f}")


if __name__ == "__main__":
    main()

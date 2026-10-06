"""Evaluate layer 3: does the injury report beat a player's recent record?

The project brief says injury is probably a key variable and to test it with
ablations rather than assume it. That is the whole point of this script: the
state-only model already knows who has been playing, so the report has to earn
its place against it.
"""

from __future__ import annotations

import numpy as np
import polars as pl

from nba_hierarchy.availability import (
    REPORT_FEATURES,
    REPORT_LEVELS,
    STATE_FEATURES,
    TARGET,
    add_report_features,
    fit_calibrated_play_model,
    fit_play_model,
    injury_era,
    predict_play_probability,
    score,
    status_rate_table,
)
from nba_hierarchy.data import load_player_games
from nba_hierarchy.roster import build_panel
from nba_hierarchy.state import add_pre_game_state

VALIDATION_SEASONS = ["2023-24", "2024-25", "2025-26"]


def main() -> None:
    panel = build_panel(pg=load_player_games())
    data = injury_era(add_report_features(add_pre_game_state(panel)))
    print(f"injury-era panel rows: {data.height:,}")
    print(f"base play rate: {data[TARGET].mean():.4f}\n")

    print("=== play rate by report status (all injury-era rows) ===")
    print(
        data.group_by("report_level")
        .agg(pl.len().alias("rows"), pl.col(TARGET).mean().round(4).alias("play_rate"))
        .with_columns(
            status=pl.col("report_level").replace_strict(
                {i: s for i, s in enumerate(REPORT_LEVELS)}
            )
        )
        .select("status", "rows", "play_rate")
        .sort("rows", descending=True)
    )

    combined = STATE_FEATURES + REPORT_FEATURES
    rows = []
    for season in VALIDATION_SEASONS:
        train = data.filter(pl.col("season") < season)
        test = data.filter(pl.col("season") == season)
        if not train.height or not test.height:
            continue
        y = test[TARGET].to_numpy().astype(int)

        base = np.full(len(y), float(train[TARGET].mean()))
        table = status_rate_table(train)
        fallback = float(train[TARGET].mean())
        status_only = np.array(
            [table.get(int(l), fallback) for l in test["report_level"]]
        )
        m_state = fit_play_model(train, STATE_FEATURES)
        m_full = fit_play_model(train, combined)
        p_state = predict_play_probability(m_state, test, STATE_FEATURES)["p_play"].to_numpy()
        p_full = predict_play_probability(m_full, test, combined)["p_play"].to_numpy()

        m_cal = fit_calibrated_play_model(train, combined)
        p_cal = m_cal.predict(test)

        for name, p in [
            ("base_rate", base),
            ("status_only", status_only),
            ("state_only", p_state),
            ("state+report", p_full),
            ("state+report+isotonic", p_cal),
        ]:
            rows.append({"season": season, "model": name, **score(p, y)})

    res = pl.DataFrame(rows)
    pl.Config.set_tbl_rows(40)
    pl.Config.set_tbl_width_chars(120)
    print("\n=== per season ===")
    print(res.select("season", "model", pl.col("log_loss").round(4),
                     pl.col("brier").round(4), pl.col("auc").round(4),
                     pl.col("ece").round(4)))
    print("\n=== mean over validation seasons ===")
    print(res.group_by("model").agg(
        pl.col("log_loss").mean().round(4),
        pl.col("brier").mean().round(4),
        pl.col("auc").mean().round(4),
        pl.col("ece").mean().round(4),
    ).sort("log_loss"))

    # Calibration of the full model on the most recent season.
    season = VALIDATION_SEASONS[-1]
    train, test = data.filter(pl.col("season") < season), data.filter(pl.col("season") == season)
    m = fit_calibrated_play_model(train, combined)
    scored = test.with_columns(p_play=pl.Series(m.predict(test)))
    print(f"\n=== calibration after isotonic, {season} ===")
    print(
        scored.with_columns(bucket=(pl.col("p_play") * 10).floor().clip(0, 9).cast(pl.Int32))
        .group_by("bucket")
        .agg(
            pl.len().alias("rows"),
            pl.col("p_play").mean().round(3).alias("predicted"),
            pl.col(TARGET).mean().round(3).alias("observed"),
        )
        .sort("bucket")
    )


if __name__ == "__main__":
    main()

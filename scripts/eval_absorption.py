"""Evaluate layer 4: do absorption features beat knowing a player's own form?

The brief requires comparison against simple baselines before claiming a model
helps, so the season mean and the trailing mean are both here. The third
baseline is the one that really matters: a mechanical allocation that shares the
team's minute budget in proportion to expected minutes, with nothing learned.
It already captures absorption by construction, so a learned model has to beat
it to be worth its machinery.
"""

from __future__ import annotations

import numpy as np
import polars as pl

from nba_hierarchy.absorption import (
    ABSORPTION_FEATURES,
    MINUTES_TARGET,
    OWN_FEATURES,
    USAGE_TARGET,
    allocation_baseline,
    fit_regressor,
    predict,
    regression_score,
)
from nba_hierarchy.pipeline import layer4_frame

TEST_SEASONS = ["2024-25", "2025-26"]


def add_season_to_date_means(df: pl.DataFrame) -> pl.DataFrame:
    """Expanding mean over the player's played games this season, excluding this one."""
    played_min = pl.when(pl.col("played")).then(pl.col(MINUTES_TARGET)).otherwise(None)
    played_usg = pl.when(pl.col("played")).then(pl.col(USAGE_TARGET)).otherwise(None)
    return df.sort(["player_id", "game_date", "game_id"]).with_columns(
        season_mean_min=played_min.cum_sum().shift(1).forward_fill().over(["player_id", "season"])
        / played_min.is_not_null().cast(pl.Int32).cum_sum().shift(1).forward_fill().over(["player_id", "season"]),
        season_mean_usg=played_usg.cum_sum().shift(1).forward_fill().over(["player_id", "season"])
        / played_usg.is_not_null().cast(pl.Int32).cum_sum().shift(1).forward_fill().over(["player_id", "season"]),
    )


def report(title: str, rows: list[dict]) -> pl.DataFrame:
    res = pl.DataFrame(rows)
    print(f"\n=== {title} ===")
    print(res.select("season", "model", pl.col("mae").round(3),
                     pl.col("rmse").round(3), pl.col("r2").round(4)))
    print("mean over test seasons:")
    print(res.group_by("model").agg(
        pl.col("mae").mean().round(3), pl.col("rmse").mean().round(3),
        pl.col("r2").mean().round(4),
    ).sort("mae"))
    return res


def main() -> None:
    data = add_season_to_date_means(layer4_frame())
    combined = OWN_FEATURES + ABSORPTION_FEATURES
    print(f"layer 4 frame: {data.height:,} rows, seasons "
          f"{sorted(data['season'].unique().to_list())}")

    min_rows, usg_rows, slices = [], [], []
    for season in TEST_SEASONS:
        train = data.filter((pl.col("season") < season) & pl.col("played"))
        test = data.filter((pl.col("season") == season) & pl.col("played"))
        if train.height < 10_000 or not test.height:
            continue
        budget = float(
            train.group_by(["game_id", "team_id"])
            .agg(pl.col(MINUTES_TARGET).sum().alias("t"))["t"]
            .mean()
        )
        test = allocation_baseline(test, budget)

        # ---- minutes
        y = test[MINUTES_TARGET].to_numpy()
        m_own = fit_regressor(train, OWN_FEATURES, MINUTES_TARGET)
        m_all = fit_regressor(train, combined, MINUTES_TARGET)
        scored = predict(m_own, test, OWN_FEATURES, "p_min_own")
        scored = predict(m_all, scored, combined, "p_min_all")
        preds = {
            "season_mean": scored["season_mean_min"].fill_null(train[MINUTES_TARGET].mean()).to_numpy(),
            "trailing_mean (ewm_min_played_8)": scored["ewm_min_played_8"].fill_null(train[MINUTES_TARGET].mean()).to_numpy(),
            "allocation_baseline": scored["alloc_minutes"].to_numpy(),
            "model_own_only": scored["p_min_own"].to_numpy(),
            "model_own+absorption": scored["p_min_all"].to_numpy(),
        }
        for name, p in preds.items():
            min_rows.append({"season": season, "model": name, **regression_score(p, y)})

        # Where absorption should matter most: teams missing a lot of minutes.
        cut = scored["expected_vacated_minutes"].quantile(0.75)
        hi = scored.filter(pl.col("expected_vacated_minutes") >= cut)
        yh = hi[MINUTES_TARGET].to_numpy()
        for name, col in [("allocation_baseline", "alloc_minutes"),
                          ("model_own_only", "p_min_own"),
                          ("model_own+absorption", "p_min_all")]:
            slices.append({"season": season, "model": name,
                           **regression_score(hi[col].to_numpy(), yh)})

        # ---- usage
        yu = test[USAGE_TARGET].to_numpy()
        u_own = fit_regressor(train, OWN_FEATURES, USAGE_TARGET)
        u_all = fit_regressor(train, combined, USAGE_TARGET)
        su = predict(u_own, test, OWN_FEATURES, "p_usg_own")
        su = predict(u_all, su, combined, "p_usg_all")
        upreds = {
            "season_mean": su["season_mean_usg"].fill_null(train[USAGE_TARGET].mean()).to_numpy(),
            "trailing_mean (ewm_usg_pct_8)": su["ewm_usg_pct_8"].fill_null(train[USAGE_TARGET].mean()).to_numpy(),
            "model_own_only": su["p_usg_own"].to_numpy(),
            "model_own+absorption": su["p_usg_all"].to_numpy(),
        }
        for name, p in upreds.items():
            usg_rows.append({"season": season, "model": name, **regression_score(p, yu)})

    report("MINUTES (players who played)", min_rows)
    report("MINUTES, top quartile of expected vacated minutes", slices)
    report("USAGE (players who played)", usg_rows)


if __name__ == "__main__":
    main()

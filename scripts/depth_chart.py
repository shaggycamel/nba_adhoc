"""The daily run: print a team's position-assigned, availability-aware depth chart.

    uv run python scripts/depth_chart.py BOS 2025-03-05
"""

from __future__ import annotations

import sys
from datetime import date

import polars as pl

from nba_hierarchy.pipeline import daily_hierarchy

POSITION_ORDER = {p: i for i, p in enumerate(("PG", "SG", "SF", "PF", "C"))}


def main(team: str = "BOS", as_of: str = "2025-03-05") -> None:
    served = daily_hierarchy(date.fromisoformat(as_of))
    pl.Config.set_tbl_rows(40)
    pl.Config.set_tbl_width_chars(150)

    chart = (
        served.filter(pl.col("team_abbreviation") == team)
        .sort([pl.col("position").replace_strict(POSITION_ORDER), "position_depth"])
        .select(
            "position",
            pl.col("position_depth").alias("dep"),
            pl.col("player_name").str.slice(0, 19).alias("player"),
            pl.col("report_status").fill_null("-").str.slice(0, 5).alias("rpt"),
            pl.col("p_play").round(2).alias("p_play"),
            pl.col("minutes_if_plays").round(1).alias("min_if"),
            pl.col("expected_minutes").round(1).alias("exp_min"),
            pl.col("expected_usage").round(3).alias("exp_usg"),
            pl.col("expected_vacated_minutes").round(1).alias("vac_min"),
            pl.col("expected_vacated_minutes_same_position").round(1).alias("vac_pos"),
            pl.col("rank_improvement").alias("rank+"),
        )
    )
    print(f"\n=== {team} hierarchy as of {as_of} ===")
    print(chart)
    print(
        f"team expected minutes: {served.filter(pl.col('team_abbreviation') == team)['expected_minutes'].sum():.0f}"
        "  (a team spends ~236)"
    )


if __name__ == "__main__":
    main(*(sys.argv[1:] or ["BOS"]))

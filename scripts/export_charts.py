"""Export a date's depth charts as a CSV and a plain-text report.

Writes both artifacts under `reports/`. Used to produce the committed
`reports/depth_charts_*.csv` and `.txt`, so those files can be regenerated
rather than being orphaned outputs.

    uv run python scripts/export_charts.py 2026-04-12
"""

from __future__ import annotations

import sys
from datetime import date
from pathlib import Path

import polars as pl

from nba_hierarchy.pipeline import daily_hierarchy

REPORTS = Path(__file__).resolve().parent.parent / "reports"
POSITION_ORDER = {p: i for i, p in enumerate(("PG", "SG", "SF", "PF", "C"))}




def chart_frame(as_of: date) -> pl.DataFrame:
    served = daily_hierarchy(as_of)
    return (
        served.with_columns(_p=pl.col("position").replace_strict(POSITION_ORDER))
        .sort(["team_abbreviation", "_p", "position_depth"])
        .select(
            # The fixture these rows describe. Without it a chart is
            # uninterpretable once separated from its filename.
            "game_date",
            "game_id",
            pl.col("team_abbreviation").alias("team"),
            pl.col("opponent"),
            pl.col("home"),
            "position",
            pl.col("position_depth").alias("depth"),
            pl.col("player_name").alias("player"),
            pl.col("height_cm").cast(pl.Int32).alias("height_cm"),
            pl.col("report_status").fill_null("").alias("injury_report"),
            pl.col("p_play").round(3).alias("p_play"),
            pl.col("minutes_if_plays").round(1).alias("minutes_if_plays"),
            pl.col("expected_minutes").round(1).alias("expected_minutes"),
            pl.col("expected_usage").round(3).alias("expected_usage"),
            pl.col("depth_rank").alias("roster_rank"),
            pl.col("available_rank").alias("available_rank"),
            pl.col("rank_improvement").alias("rank_gain"),
            pl.col("expected_vacated_minutes").round(1).alias("team_vacated_min"),
            pl.col("expected_vacated_minutes_same_position")
            .round(1)
            .alias("vacated_min_same_pos"),
        )
        .with_columns(
            # Meaningless where the player has almost no chance of playing:
            # every unavailable player ties at zero expected minutes, so their
            # order among each other carries no information.
            rank_gain=pl.when(pl.col("p_play") > 0.05)
            .then(pl.col("rank_gain"))
            .otherwise(None)
        )
    )


def text_report(df: pl.DataFrame, as_of: date) -> str:
    """A plain-text companion to the CSV: how to read it, and what to distrust."""
    teams = df.group_by("team").agg(
        pl.col("expected_minutes").sum().round(0).alias("exp"),
        (pl.col("injury_report").is_in(["Out", "Doubtful"])).sum().alias("out"),
    ).sort("team")
    lines = [
        f"NBA team hierarchy -- depth charts for {as_of:%d %B %Y}",
        "=" * 64,
        "",
        f"{df.height} players across {df['team'].n_unique()} teams. Full table in "
        f"depth_charts_{as_of}.csv alongside this file.",
        "",
        "COLUMNS",
        "  position/depth        assigned position, and rank within it on this team",
        "  p_play                calibrated probability the player takes the floor",
        "  minutes_if_plays      expected minutes conditional on playing",
        "  expected_minutes      p_play x minutes_if_plays -- the headline number",
        "  expected_usage        expected share of team possessions while on court",
        "  roster_rank           standing by trailing minutes across the full roster",
        "  available_rank        standing among team-mates expected to be available",
        "  rank_gain             roster_rank minus available_rank -- the absorption",
        "                        signal. Blank where p_play is near zero, since every",
        "                        unavailable player ties at zero expected minutes.",
        "  team_vacated_min      minutes the team expects to go unclaimed by their",
        "  vacated_min_same_pos  usual owner, in total and at this player's position",
        "",
        "WHAT TO TRUST",
        "  Expected minutes are the solid output: R2 0.787 and mean absolute error",
        "  4.0 minutes against what players actually played, on held-out seasons.",
        "",
        "  Expected usage is weak. It barely beats a trailing average (R2 0.342",
        "  against 0.331); game-level usage is mostly noise once minutes are known.",
        "  Lean on the minutes column.",
        "",
        "  PG/SG and SF/PF are a convention, not a label. No source in the database",
        "  carries the five positions. Guard/forward/centre is learned and holds 97%",
        "  game to game; the split within guards and within forwards is imposed,",
        "  holds 87%, and about a quarter of those calls rest on a margin thin",
        "  enough to be arbitrary. Treat C and the coarse slot as reliable.",
        "",
        "  Read rank_gain together with vacated_min_same_pos. A large gain next to a",
        "  large same-position vacancy is absorption proper. A large gain with almost",
        "  nothing vacated usually means the player is himself returning from a spell",
        "  out: his trailing minutes have decayed, so he sits low on the full roster",
        "  but first among those available.",
        "",
        "  Team totals do not add up. Each player is predicted independently, so a",
        "  healthy team lands near its ~236-minute budget while a heavily depleted",
        "  one falls far short. On a depleted team read the ordering and the rank",
        "  gains rather than the absolute minutes. Rescaling each team to the budget",
        "  was tested and did not improve per-player accuracy, so it is left alone.",
        "",
        "TEAM TOTALS (expected minutes against a ~236 budget; 'out' counts players",
        "listed Out or Doubtful)",
        "",
    ]
    lines += [
        f"  {r['team']:<4} {r['exp']:>6.0f}   out {r['out']:>2}"
        for r in teams.iter_rows(named=True)
    ]
    lines += [
        "",
        f"Generated by scripts/export_charts.py {as_of}. See README.md for the",
        "model's layers and its full evaluation.",
        "",
    ]
    return "\n".join(lines)


def main(as_of_str: str = "2026-04-12") -> None:
    as_of = date.fromisoformat(as_of_str)
    df = chart_frame(as_of)
    REPORTS.mkdir(exist_ok=True)
    csv_path = REPORTS / f"depth_charts_{as_of}.csv"
    txt_path = REPORTS / f"depth_charts_{as_of}.txt"
    df.write_csv(csv_path)
    txt_path.write_text(text_report(df, as_of))
    print(f"{df.height} rows -> {csv_path.name}, {txt_path.name}")


if __name__ == "__main__":
    main(*(sys.argv[1:] or []))

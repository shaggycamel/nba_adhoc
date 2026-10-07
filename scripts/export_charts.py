"""Export a date's depth charts as CSV, plain text, and a standalone HTML page.

Writes all three artifacts under `reports/`, so the committed
`reports/depth_charts_*` files can be regenerated rather than being orphaned
outputs. The HTML page covers every team on the slate; the CSV is the same data
for scripting; the text file is the glossary and per-team totals.

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


# ---------------------------------------------------------------- HTML

_FONT = "font:13px -apple-system,Segoe UI,Roboto,Helvetica,Arial,sans-serif"
_PROSE = "font:14px/1.55 -apple-system,Segoe UI,Roboto,Helvetica,Arial,sans-serif"
_NUMERIC = {
    "depth", "p_play", "minutes_if_plays", "expected_minutes", "expected_usage",
    "rank_gain", "roster_rank", "available_rank", "team_vacated_min",
    "vacated_min_same_pos", "height_cm",
}
_CHART_COLS = [
    "position", "depth", "player", "height_cm", "injury_report", "p_play",
    "minutes_if_plays", "expected_minutes", "expected_usage", "rank_gain",
]


def _html_table(df: pl.DataFrame, cols: list[str]) -> str:
    head = "".join(
        f'<th align="{"right" if c in _NUMERIC else "left"}">{c.replace("_", " ")}</th>'
        for c in cols
    )
    rows = []
    for i, r in enumerate(df.select(cols).iter_rows(named=True)):
        cells = "".join(
            f'<td align="right">{"" if r[c] is None else r[c]}</td>'
            if c in _NUMERIC
            else f'<td>{"" if r[c] is None else r[c]}</td>'
            for c in cols
        )
        stripe = ' bgcolor="#f7f7f7"' if i % 2 else ""
        rows.append(f"<tr{stripe}>{cells}</tr>")
    return (
        f'<table border="0" cellspacing="0" cellpadding="6" '
        f'style="border-collapse:collapse;{_FONT};margin:6px 0 18px">'
        f'<thead><tr bgcolor="#2f2f2f" style="color:#fff">{head}</tr></thead>'
        f'<tbody>{"".join(rows)}</tbody></table>'
    )


def html_report(df: pl.DataFrame, as_of: date) -> str:
    """A standalone page: one depth chart per team on the slate, plus caveats."""
    teams = df["team"].unique().sort().to_list()
    charts = []
    for team in teams:
        t = df.filter(pl.col("team") == team)
        first = t.row(0, named=True)
        where = "vs" if first["home"] else "@"
        charts.append(
            f'<h2 id="{team}" style="{_PROSE};font-size:17px;font-weight:600;'
            f'margin:28px 0 0;padding-top:12px;border-top:1px solid #ddd">'
            f'{team} {where} {first["opponent"]}</h2>'
            f'<div style="{_FONT};font-size:12px;color:#666">game {first["game_id"]}'
            f' &middot; team expected minutes {t["expected_minutes"].sum():.0f}'
            f' of a ~236 budget</div>'
            + _html_table(t, _CHART_COLS)
        )
    nav = " &middot; ".join(f'<a href="#{t}" style="color:#06c">{t}</a>' for t in teams)
    return f"""<!doctype html>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width,initial-scale=1">
<title>NBA depth charts {as_of}</title>
<body style="margin:0;padding:24px 16px;background:#fff;color:#222">
<div style="max-width:1000px">
<h1 style="{_PROSE};font-size:22px;margin:0 0 4px">NBA team hierarchy &mdash;
{as_of:%d %B %Y}</h1>
<p style="{_PROSE};color:#444;margin:4px 0 14px">{df.height} players across
{len(teams)} teams playing this date. Same data as
<code>depth_charts_{as_of}.csv</code>; see <code>depth_charts_{as_of}.txt</code>
for the column glossary and per-team totals.</p>
<p style="{_FONT};color:#555">{nav}</p>

<h2 style="{_PROSE};font-size:17px;font-weight:600;margin:26px 0 4px;
padding-top:12px;border-top:1px solid #ddd">What to trust</h2>
<ul style="{_PROSE};color:#222">
<li><b>Expected minutes</b> (p_play &times; minutes_if_plays) are the solid
output: R&sup2; 0.787 and mean absolute error 4.0 minutes against what players
actually played, on held-out seasons.</li>
<li><b>Expected usage is weak</b> &mdash; it barely beats a trailing average
(R&sup2; 0.342 against 0.331). Lean on the minutes column.</li>
<li><b>PG/SG and SF/PF are a convention, not a label.</b> No source carries the
five positions. Guard/forward/centre is learned and holds 97% game to game; the
split within guards and within forwards is imposed, holds 87%, and about a
quarter of those calls rest on a margin thin enough to be arbitrary.</li>
<li><b>Blank rank gain</b> means the player has almost no chance of playing, so
the figure carries no information &mdash; every unavailable player ties at zero
expected minutes.</li>
<li><b>Team totals do not add up.</b> Each player is predicted independently, so
a healthy team lands near its ~236-minute budget while a heavily depleted one
falls far short. On a depleted team read the ordering and the rank gains rather
than the absolute minutes.</li>
</ul>
{"".join(charts)}
<p style="{_FONT};font-size:12px;color:#666;margin-top:24px">Generated by
<code>scripts/export_charts.py {as_of}</code>. See README.md for the model's
layers and its full evaluation.</p>
</div></body>"""


def main(as_of_str: str = "2026-04-12") -> None:
    as_of = date.fromisoformat(as_of_str)
    df = chart_frame(as_of)
    REPORTS.mkdir(exist_ok=True)
    stem = f"depth_charts_{as_of}"
    df.write_csv(REPORTS / f"{stem}.csv")
    (REPORTS / f"{stem}.txt").write_text(text_report(df, as_of))
    (REPORTS / f"{stem}.html").write_text(html_report(df, as_of))
    print(f"{df.height} rows -> {stem}.{{csv,txt,html}}")


if __name__ == "__main__":
    main(*(sys.argv[1:] or []))

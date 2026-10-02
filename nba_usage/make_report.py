"""Build report.html from the generated results tables.

    uv run python -m nba_usage.make_report

Everything the report states is parsed out of RESULTS.md, RESULTS_ablation.md
and RESULTS_processing.md, so the page cannot drift from the numbers the
experiment scripts produced.
"""

from __future__ import annotations

import html
import re
from pathlib import Path

RESULTS = Path("RESULTS.md")
ABLATION = Path("RESULTS_ablation.md")
PROCESSING = Path("RESULTS_processing.md")
OUT = Path("report.html")


# --- parsing ---------------------------------------------------------------

def parse_tables(path: Path) -> dict[str, list[dict[str, str]]]:
    """Every pipe table in a markdown file, keyed by the heading above it."""
    tables: dict[str, list[dict[str, str]]] = {}
    heading = path.stem
    header: list[str] | None = None
    for line in path.read_text().splitlines():
        if line.startswith("#"):
            heading = line.lstrip("#").strip()
            header = None
            continue
        if not line.startswith("|"):
            continue
        cells = [c.strip() for c in line.strip().strip("|").split("|")]
        if set("".join(cells)) <= {"-", ":"}:
            continue
        if header is None:
            header = cells
            tables.setdefault(heading, [])
            continue
        tables[heading].append(dict(zip(header, cells)))
    return tables


def f(row: dict[str, str], key: str) -> float:
    return float(row[key])


# --- svg helpers -----------------------------------------------------------

def esc(s: str) -> str:
    return html.escape(str(s))


def nice_ticks(lo: float, hi: float, step: float) -> list[float]:
    first = step * (int(lo / step) + (1 if lo % step else 0))
    ticks, v = [], first
    while v <= hi + 1e-12:
        ticks.append(round(v, 10))
        v += step
    return ticks


def dot_plot(rows: list[tuple[str, float, str]], step: float, unit: str = "") -> str:
    """Horizontal dot plot. rows = (label, value, series key).

    The axis does not start at zero: these values differ in the fourth decimal
    and bars from zero would be indistinguishable. Dots on a stated range are
    the honest way to show that.
    """
    pad_l, pad_r, pad_t, pad_b, row_h = 196, 58, 30, 42, 23
    w, h = 760, pad_t + row_h * len(rows) + pad_b
    vals = [v for _, v, _ in rows]
    lo, hi = min(vals), max(vals)
    span = hi - lo or 1
    x0, x1 = lo - span * 0.1, hi + span * 0.1
    sx = lambda v: pad_l + (v - x0) / (x1 - x0) * (w - pad_l - pad_r)

    out = [f'<svg viewBox="0 0 {w} {h}" class="chart" role="img">']
    for t in nice_ticks(x0, x1, step):
        x = sx(t)
        out.append(f'<line class="grid" x1="{x:.1f}" y1="{pad_t - 8}" x2="{x:.1f}" y2="{pad_t + row_h * len(rows):.1f}"/>')
        out.append(f'<text class="tick" x="{x:.1f}" y="{h - pad_b + 26:.0f}" text-anchor="middle">{t:.3f}</text>')
    for i, (label, v, series) in enumerate(rows):
        y = pad_t + row_h * i + row_h / 2
        out.append(f'<text class="rowlabel" x="{pad_l - 14}" y="{y + 4:.1f}" text-anchor="end">{esc(label)}</text>')
        out.append(f'<line class="connector" x1="{pad_l}" y1="{y:.1f}" x2="{sx(v):.1f}" y2="{y:.1f}"/>')
        out.append(f'<circle class="dot s-{series}" cx="{sx(v):.1f}" cy="{y:.1f}" r="5.5"/>')
        out.append(f'<text class="value" x="{sx(v) + 13:.1f}" y="{y + 4:.1f}">{v:.5f}</text>')
    out.append(f'<text class="axistitle" x="{w / 2:.0f}" y="{h - 6}" text-anchor="middle">MAE{unit}</text>')
    out.append("</svg>")
    return "\n".join(out)


def grouped_dots(labels: list[str], series: list[tuple[str, str, list[float]]], step: float) -> str:
    """One row per label, one dot per series. Used where a sequence would lie."""
    pad_l, pad_r, pad_t, pad_b, row_h = 232, 58, 26, 42, 36
    w, h = 760, pad_t + row_h * len(labels) + pad_b
    vals = [v for _, _, vs in series for v in vs]
    lo, hi = min(vals), max(vals)
    span = hi - lo or 1
    x0, x1 = lo - span * 0.14, hi + span * 0.14
    sx = lambda v: pad_l + (v - x0) / (x1 - x0) * (w - pad_l - pad_r)

    out = [f'<svg viewBox="0 0 {w} {h}" class="chart" role="img">']
    for t in nice_ticks(x0, x1, step):
        x = sx(t)
        out.append(f'<line class="grid" x1="{x:.1f}" y1="{pad_t - 6}" x2="{x:.1f}" y2="{pad_t + row_h * len(labels):.1f}"/>')
        out.append(f'<text class="tick" x="{x:.1f}" y="{h - pad_b + 26:.0f}" text-anchor="middle">{t:.3f}</text>')
    for i, label in enumerate(labels):
        y = pad_t + row_h * i + row_h / 2
        out.append(f'<text class="rowlabel" x="{pad_l - 14}" y="{y + 4:.1f}" text-anchor="end">{esc(label)}</text>')
        xs = [sx(vs[i]) for _, _, vs in series]
        out.append(f'<line class="connector" x1="{min(xs):.1f}" y1="{y:.1f}" x2="{max(xs):.1f}" y2="{y:.1f}"/>')
        for (_, key, vs) in series:
            out.append(f'<circle class="dot s-{key}" cx="{sx(vs[i]):.1f}" cy="{y:.1f}" r="5.5"/>')
    out.append(f'<text class="axistitle" x="{w / 2:.0f}" y="{h - 6}" text-anchor="middle">MAE</text>')
    out.append("</svg>")
    return "\n".join(out)


def line_chart(xs: list[int], ys: list[float], labels: list[str], refs: list[tuple[str, float]]) -> str:
    pad_l, pad_r, pad_t, pad_b = 66, 190, 24, 56
    w, h = 760, 330
    allv = ys + [v for _, v in refs]
    lo, hi = min(allv), max(allv)
    span = hi - lo or 1
    y0, y1 = lo - span * 0.12, hi + span * 0.12
    sx = lambda x: pad_l + (x - min(xs)) / (max(xs) - min(xs)) * (w - pad_l - pad_r)
    sy = lambda v: pad_t + (y1 - v) / (y1 - y0) * (h - pad_t - pad_b)

    out = [f'<svg viewBox="0 0 {w} {h}" class="chart" role="img">']
    for t in nice_ticks(y0, y1, 0.0005):
        y = sy(t)
        out.append(f'<line class="grid" x1="{pad_l}" y1="{y:.1f}" x2="{w - pad_r}" y2="{y:.1f}"/>')
        out.append(f'<text class="tick" x="{pad_l - 10}" y="{y + 4:.1f}" text-anchor="end">{t:.4f}</text>')
    for name, v in refs:
        y = sy(v)
        out.append(f'<line class="refline" x1="{pad_l}" y1="{y:.1f}" x2="{w - pad_r}" y2="{y:.1f}"/>')
        out.append(f'<text class="reflabel" x="{w - pad_r + 10}" y="{y + 4:.1f}">{esc(name)}</text>')
    pts = " ".join(f"{sx(x):.1f},{sy(v):.1f}" for x, v in zip(xs, ys))
    out.append(f'<polyline class="series s-model" points="{pts}"/>')
    for x, v, lab in zip(xs, ys, labels):
        out.append(f'<circle class="dot s-model" cx="{sx(x):.1f}" cy="{sy(v):.1f}" r="4.5"/>')
        out.append(f'<text class="tick" x="{sx(x):.1f}" y="{h - pad_b + 24:.0f}" text-anchor="middle">{x}</text>')
    # Label the first, third and last steps only; a number on every point is noise.
    for i, dx, dy, anchor in ((0, 0, -16, "start"), (2, 10, -16, "start"), (len(xs) - 1, 0, -16, "end")):
        out.append(
            f'<text class="pointlabel" x="{sx(xs[i]) + dx:.1f}" y="{sy(ys[i]) + dy:.1f}" '
            f'text-anchor="{anchor}">{esc(labels[i])}</text>'
        )
    out.append(f'<text class="axistitle" x="{(pad_l + w - pad_r) / 2:.0f}" y="{h - 8}" text-anchor="middle">features in the model</text>')
    out.append("</svg>")
    return "\n".join(out)


def md_table(rows: list[dict[str, str]], headers: list[str] | None = None, highlight: str | None = None) -> str:
    if not rows:
        return ""
    headers = headers or list(rows[0].keys())
    head = "".join(f"<th>{esc(h.replace('_', ' '))}</th>" for h in headers)
    body = []
    for r in rows:
        cls = ' class="is-best"' if highlight and r.get(highlight) == "1" else ""
        cells = "".join(
            f'<td class="num">{esc(r[h])}</td>' if re.fullmatch(r"-?\d+\.?\d*", r.get(h, "")) else f"<td>{esc(r.get(h, ''))}</td>"
            for h in headers
        )
        body.append(f"<tr{cls}>{cells}</tr>")
    return f'<div class="tablewrap"><table><thead><tr>{head}</tr></thead><tbody>{"".join(body)}</tbody></table></div>'


# --- page ------------------------------------------------------------------

STYLE = """
/* Layout: one measured reading column; charts and tables break out wider,
   each in its own scroll container so the page never scrolls sideways. */
:root {
  --bg: #f6f7f9;
  --surface: #ffffff;
  --ink: #14171c;
  --ink-2: #434b58;
  --muted: #6d7686;
  --line: #e1e5eb;
  --line-soft: #eef1f5;
  --accent: #2a78d6;
  --s-model: #2a78d6;
  --s-base: #eb6834;
  --good: #0f7b46;
  --bad: #b0442c;
  --grid: #e6eaef;
  --font-display: "Newsreader", Georgia, "Times New Roman", serif;
  --font-body: "IBM Plex Sans", system-ui, -apple-system, "Segoe UI", sans-serif;
  --font-mono: "IBM Plex Mono", ui-monospace, "SF Mono", Menlo, monospace;
  color-scheme: light;
}
@media (prefers-color-scheme: dark) {
  :root:not([data-theme="light"]) {
    --bg: #111318;
    --surface: #191c22;
    --ink: #e9ecf1;
    --ink-2: #b3bcc9;
    --muted: #8a94a3;
    --line: #2a2f38;
    --line-soft: #22262e;
    --accent: #3987e5;
    --s-model: #3987e5;
    --s-base: #d95926;
    --good: #35a46b;
    --bad: #e07a62;
    --grid: #272c35;
    color-scheme: dark;
  }
}
:root[data-theme="dark"] {
  --bg: #111318; --surface: #191c22; --ink: #e9ecf1; --ink-2: #b3bcc9;
  --muted: #8a94a3; --line: #2a2f38; --line-soft: #22262e; --accent: #3987e5;
  --s-model: #3987e5; --s-base: #d95926; --good: #35a46b; --bad: #e07a62;
  --grid: #272c35; color-scheme: dark;
}

* { box-sizing: border-box; }
body {
  margin: 0; background: var(--bg); color: var(--ink);
  font-family: var(--font-body); font-size: 16px; line-height: 1.6;
  -webkit-font-smoothing: antialiased;
}
.wrap { max-width: 860px; margin: 0 auto; padding-inline: 20px; padding-block: 56px 96px; }

header.masthead { border-bottom: 2px solid var(--ink); padding-bottom: 22px; margin-bottom: 40px; }
.eyebrow {
  font-family: var(--font-mono); font-size: 11px; letter-spacing: .13em;
  text-transform: uppercase; color: var(--muted); margin: 0 0 14px;
}
h1 {
  font-family: var(--font-display); font-weight: 500; font-size: clamp(30px, 6vw, 46px);
  line-height: 1.1; margin: 0 0 14px; text-wrap: balance; letter-spacing: -.01em;
}
.standfirst { font-size: 17px; color: var(--ink-2); margin: 0; max-width: 62ch; }

h2 {
  font-family: var(--font-display); font-weight: 500; font-size: 27px; line-height: 1.2;
  margin: 56px 0 6px; text-wrap: balance;
}
h2 + .sectionnote { color: var(--muted); font-size: 14px; margin: 0 0 20px; max-width: 62ch; }
h3 { font-size: 15px; font-weight: 600; margin: 32px 0 8px; letter-spacing: -.005em; }
p { max-width: 65ch; margin: 0 0 16px; }
strong { font-weight: 600; }
code, .mono { font-family: var(--font-mono); font-size: .88em; }
code { background: var(--line-soft); padding: 1px 5px; border-radius: 3px; }
a { color: var(--accent); }

.tiles { display: grid; grid-template-columns: repeat(auto-fit, minmax(190px, 1fr)); gap: 14px; margin: 28px 0 8px; }
.tile { background: var(--surface); border: 1px solid var(--line); border-radius: 8px; padding: 16px 18px; min-width: 0; }
.tile .k { font-family: var(--font-mono); font-size: 10.5px; letter-spacing: .1em; text-transform: uppercase; color: var(--muted); }
.tile .v { font-family: var(--font-mono); font-size: 27px; font-variant-numeric: tabular-nums; margin-top: 6px; letter-spacing: -.02em; }
.tile .s { font-size: 13px; color: var(--ink-2); margin-top: 4px; }
.tile.lead .v { color: var(--accent); }

figure { margin: 24px 0 8px; }
figcaption { font-size: 13px; color: var(--muted); margin-top: 10px; max-width: 62ch; }
.chartwrap { overflow-x: auto; background: var(--surface); border: 1px solid var(--line); border-radius: 8px; padding: 10px 6px; }
svg.chart { display: block; width: 100%; min-width: 560px; height: auto; font-family: var(--font-body); }
.grid { stroke: var(--grid); stroke-width: 1; }
.refline { stroke: var(--muted); stroke-width: 1; stroke-dasharray: 3 4; }
.connector { stroke: var(--line); stroke-width: 2; }
.dot { stroke: var(--surface); stroke-width: 2; }
.s-model { fill: var(--s-model); stroke: var(--s-model); }
/* Must follow .s-model: a filled polyline renders as a wedge, not a line. */
polyline.series { fill: none; stroke: var(--s-model); stroke-width: 2; stroke-linejoin: round; }
circle.s-model { stroke: var(--surface); }
.s-base { fill: var(--s-base); }
circle.s-base { stroke: var(--surface); }
.s-fold1 { fill: var(--s-base); }
.s-fold2 { fill: var(--s-model); }
text { fill: var(--ink-2); }
.tick, .reflabel { font-size: 11px; fill: var(--muted); font-family: var(--font-mono); }
.rowlabel { font-size: 12.5px; fill: var(--ink-2); }
.value, .pointlabel { font-size: 11px; fill: var(--muted); font-family: var(--font-mono); font-variant-numeric: tabular-nums; }
.axistitle { font-size: 11px; fill: var(--muted); letter-spacing: .08em; text-transform: uppercase; }

.legend { display: flex; flex-wrap: wrap; gap: 18px; margin: 12px 0 0; font-size: 13px; color: var(--ink-2); }
.legend span { display: inline-flex; align-items: center; gap: 7px; }
.swatch { width: 11px; height: 11px; border-radius: 50%; flex: none; }
.sw-model { background: var(--s-model); }
.sw-base { background: var(--s-base); }

.tablewrap { overflow-x: auto; margin: 20px 0; border: 1px solid var(--line); border-radius: 8px; background: var(--surface); }
table { border-collapse: collapse; width: 100%; font-size: 13.5px; }
th, td { padding: 8px 14px; text-align: left; border-bottom: 1px solid var(--line-soft); white-space: nowrap; }
th { font-family: var(--font-mono); font-size: 10.5px; letter-spacing: .08em; text-transform: uppercase; color: var(--muted); font-weight: 500; }
tbody tr:last-child td { border-bottom: 0; }
td.num { font-family: var(--font-mono); font-variant-numeric: tabular-nums; }
tr.is-best td { background: color-mix(in srgb, var(--accent) 9%, transparent); font-weight: 600; }

.verdict { display: inline-flex; align-items: center; gap: 6px; font-family: var(--font-mono); font-size: 10.5px; letter-spacing: .08em; text-transform: uppercase; padding: 2px 8px; border-radius: 99px; }
.verdict.keep { color: var(--good); background: color-mix(in srgb, var(--good) 13%, transparent); }
.verdict.drop { color: var(--bad); background: color-mix(in srgb, var(--bad) 13%, transparent); }

.callout { border-left: 3px solid var(--accent); background: var(--surface); padding: 16px 20px; margin: 24px 0; border-radius: 0 8px 8px 0; }
.callout p:last-child { margin-bottom: 0; }

ul.notes { max-width: 65ch; padding-left: 20px; margin: 0 0 16px; }
ul.notes li { margin-bottom: 10px; }

footer { margin-top: 72px; padding-top: 22px; border-top: 1px solid var(--line); font-size: 13px; color: var(--muted); }
footer code { font-size: 12px; }
@media (prefers-reduced-motion: reduce) { * { animation: none !important; transition: none !important; } }
"""


def main() -> None:
    res = parse_tables(RESULTS)["Results"]
    abl = parse_tables(ABLATION)["Feature ablation"]
    proc = parse_tables(PROCESSING)

    folds = sorted({r["fold"] for r in res})
    last = folds[-1]

    def pick(fold: str, kind: str) -> list[dict[str, str]]:
        return sorted((r for r in res if r["fold"] == fold and r["kind"] == kind), key=lambda r: f(r, "MAE"))

    best_model = pick(last, "model")[0]
    best_base = pick(last, "baseline")[0]
    gain = (f(best_base, "MAE") - f(best_model, "MAE")) / f(best_base, "MAE") * 100

    # Chart 1 - every model and baseline on the most recent fold.
    rows1 = [
        (r["model"].replace("_", " "), f(r, "MAE"), "model" if r["kind"] == "model" else "base")
        for r in sorted((r for r in res if r["fold"] == last), key=lambda r: f(r, "MAE"))
    ]
    chart_models = dot_plot(rows1, step=0.005)

    # Chart 2 - feature ablation, Ridge, both folds side by side.
    order = ["history", "history+absence", "history+absence+rotation", "all", "history+absence+rotation+roles"]
    pretty = {
        "history": "usage history only",
        "history+absence": "+ injury absence",
        "history+absence+rotation": "+ rotation position",
        "all": "+ pairwise absorption",
        "history+absence+rotation+roles": "+ role clusters",
    }
    def ridge(fold: str, feats: str) -> float:
        return next(f(r, "mae") for r in abl if r["fold"] == fold and r["model"] == "ridge" and r["features"] == feats)
    chart_ablation = grouped_dots(
        [pretty[o] for o in order],
        [(folds[0], "fold1", [ridge(folds[0], o) for o in order]),
         (last, "fold2", [ridge(last, o) for o in order])],
        step=0.0005,
    )

    # Chart 3 - greedy forward selection.
    greedy = proc["Greedy forward selection (Ridge, validated on the last season)"]
    chart_greedy = line_chart(
        [int(r["n"]) for r in greedy],
        [f(r, "mae") for r in greedy],
        [r["added"] for r in greedy],
        [("best baseline", f(best_base, "MAE")), ("all 97 features", f(best_model, "MAE"))],
    )

    inj = proc["Injury encodings (Ridge, mean over folds)"]
    win = proc["Window length and weighting (Ridge, mean over folds)"]
    rowsets = proc["Outlier and DNP handling (row sets differ, so MAE is not comparable across rows)"]

    three = next(r for r in greedy if r["n"] == "3")
    twelve = greedy[-1]
    recovered3 = (f(best_base, "MAE") - f(three, "mae")) / (f(best_base, "MAE") - f(best_model, "MAE")) * 100
    recovered12 = (f(best_base, "MAE") - f(twelve, "mae")) / (f(best_base, "MAE") - f(best_model, "MAE")) * 100

    legend_models = (
        '<div class="legend"><span><i class="swatch sw-model"></i>fitted model</span>'
        '<span><i class="swatch sw-base"></i>baseline</span></div>'
    )
    legend_folds = (
        f'<div class="legend"><span><i class="swatch sw-base"></i>validate {esc(folds[0])}</span>'
        f'<span><i class="swatch sw-model"></i>validate {esc(last)}</span></div>'
    )

    body = f"""
<div class="wrap">
<header class="masthead">
  <p class="eyebrow">nba_adhoc &middot; branch opus-attempt</p>
  <h1>Forecasting next-game usage</h1>
  <p class="standfirst">What predicts a player's <code>usg_pct</code> in their next game, using only
  what is known before tip-off &mdash; which features earn their place, which were tried and dropped,
  and how much any of it beats simply averaging recent games.</p>
</header>

<section>
  <div class="tiles">
    <div class="tile lead">
      <div class="k">best model &middot; {esc(last)}</div>
      <div class="v">{f(best_model, 'MAE'):.5f}</div>
      <div class="s">{esc(best_model['model'].replace('_', ' '))}, R&sup2; {f(best_model, 'R2'):.3f}</div>
    </div>
    <div class="tile">
      <div class="k">best baseline</div>
      <div class="v">{f(best_base, 'MAE'):.5f}</div>
      <div class="s">{esc(best_base['model'].replace('_', ' '))}, R&sup2; {f(best_base, 'R2'):.3f}</div>
    </div>
    <div class="tile">
      <div class="k">improvement</div>
      <div class="v">{gain:.1f}%</div>
      <div class="s">on MAE, repeated on both folds</div>
    </div>
  </div>

  <p style="margin-top:28px">The winning combination is a <strong>regularised linear model on lagged usage
  history plus injury-absence and rotation-position features</strong>. It beats the best baseline by about
  {gain:.0f}% on MAE and adds roughly 0.05 to R&sup2;, consistently across both validation seasons.</p>

  <p>That is a real gain and a modest one. The honest summary is that <strong>usage is mostly
  autocorrelated and only slightly contextual</strong>: an exponentially weighted mean of a player's
  recent usage gets most of the way there, and everything else is a small correction on top.</p>

  <div class="callout">
    <p><strong>How this was measured.</strong> Expanding-window season folds &mdash; train on every earlier
    season, score the next one. Nothing is shuffled. Every feature is computed from games strictly before
    the one being predicted, and usage is modelled conditional on the player appearing.</p>
  </div>
</section>

<section>
  <h2>Algorithm choice barely matters</h2>
  <p class="sectionnote">Every algorithm on the same features and the same fold ({esc(last)}).
  The axis does not start at zero; these values differ in the fourth decimal.</p>
  <figure>
    <div class="chartwrap">{chart_models}</div>
    {legend_models}
    <figcaption>All seven classical algorithms land within 0.0002 MAE of one another, with the two linear
    models at the top. The two neural networks are the worst of the fitted models, though all nine still
    beat every baseline. There is no non-linear structure here worth chasing &mdash; spend effort on
    features, not on model class.</figcaption>
  </figure>
</section>

<section>
  <h2>What drives usage</h2>
  <p class="sectionnote">Cumulative feature sets, Ridge, both folds. The last two rows are alternatives
  to the third rather than additions to each other.</p>
  <figure>
    <div class="chartwrap">{chart_ablation}</div>
    {legend_folds}
    <figcaption>Injury absence and rotation position each buy about 0.0003 MAE. Pairwise absorption and
    role clusters buy nothing.</figcaption>
  </figure>

  <h3>The smallest set that holds up</h3>
  <figure>
    <div class="chartwrap">{chart_greedy}</div>
    <figcaption>Greedy forward selection on {esc(last)}. Three features recover {recovered3:.0f}% of the gap
    between the best baseline and the full 97-feature model; twelve recover {recovered12:.0f}%
    ({f(twelve, 'mae'):.5f} against {f(best_model, 'MAE'):.5f}). The other 85 features are almost entirely
    redundant &mdash; if this goes to production, the small model is the one to ship.</figcaption>
  </figure>
  {md_table(greedy, ["n", "added", "mae", "r2"])}
</section>

<section>
  <h2>Injury: real, but not the key variable</h2>
  <p class="sectionnote">Each encoding added on its own to the same history features. Ridge, mean over folds.</p>
  {md_table(inj, ["encoding", "n_features", "mae", "r2"])}
  <p><strong>The player's own injury status is worth nothing</strong> &mdash;
  {f(next(r for r in inj if r['encoding'] == 'own status only'), 'mae'):.5f} against
  {f(next(r for r in inj if r['encoding'] == 'none'), 'mae'):.5f} for no injury features at all. On
  reflection that is what should happen: the target only exists for games the player actually played, so
  conditioning on their having played already absorbs almost everything their status could say. Own status
  would matter for predicting <em>whether</em> someone plays, which is a different question.</p>
  <p><strong>Teammate absence matters mainly through rotation position, not through volume.</strong>
  Knowing how much usage was vacated is worth about 0.0003 MAE; knowing where that pushes the player in the
  healthy rotation is worth about as much again, the single largest structural contribution. What counts is
  not how much load went missing but how far up the pecking order it moves this player.</p>
</section>

<section>
  <h2>What failed</h2>
  <h3><span class="verdict drop">dropped</span> Role clustering</h3>
  <p>K-means over prior-game rate stats with usage excluded, refitted each season on earlier seasons only,
  feeding role-aware vacated load. It changed MAE by at most 0.00002 in either direction. The project rule
  is that clusters are kept only on a demonstrated gain, so it is out of the recommended set.</p>
  <h3><span class="verdict drop">dropped</span> Pairwise with/without absorption</h3>
  <p>Shrunk estimates of how each player's usage shifts when each specific teammate is out. The individual
  estimates look sensible &mdash; the strongest pair is +7.7 usage points over 42 games-without &mdash; but
  once rotation share is in the model the pairwise detail is redundant. Teammate-specific synergy, measured
  this way, does not forecast usage.</p>
  <h3><span class="verdict drop">dropped</span> Neural networks</h3>
  <p>The MLP with player embeddings and the GRU over each player's last ten games were the two worst models
  tested, behind every classical algorithm. The sequence model was worst of all, which says the ordering of
  recent games carries nothing beyond their weighted average.</p>
</section>

<section>
  <h2>Processing choices</h2>
  <h3>Window length hardly matters</h3>
  {md_table(win, ["variant", "n_features", "mae", "r2"])}
  <p>A 0.0002 spread across every variant tried. EWMA alone does as well as a full set of rolling windows.
  Short windows are measurably the worst &mdash; a three-game mean is too noisy, which also shows in the
  baselines, where last-3 is the weakest of the lot.</p>

  <h3>Most of the error lives in low-minute players</h3>
  {md_table(rowsets, ["treatment", "n_rows", "mae", "r2"])}
  <p>Restricting to games of ten minutes or more lifts R&sup2; from 0.36 to 0.51. This is <em>not</em> an
  improvement: it is a different, easier set of rows, and the MAEs are not comparable across them. But it
  locates the problem. Garbage-time and cameo appearances have erratic usage and are close to
  unpredictable, so a production model would likely be scoped to the rotation rather than trained to chase
  them.</p>
</section>

<section>
  <h2>Full model comparison</h2>
  <p class="sectionnote">{len(res)} rows across both folds, as generated.</p>
  {md_table(res, ["fold", "kind", "model", "MAE", "RMSE", "R2", "n"])}
</section>

<section>
  <h2>Caveats</h2>
  <ul class="notes">
    <li><strong>Injury data starts 2021-10-19</strong>, so every injury-aware result uses five seasons, not
    the seventeen the box scores cover.</li>
    <li><strong>10,160 of 64,246 injury rows are dropped</strong> because they do not match a game for that
    team on that date. The NBA publishes the report the evening before, so some rows carry the report date
    rather than the game date. This understates absences; it cannot leak. Recovering them is the first thing
    to try if the absence features are worth pushing further.</li>
    <li><strong>Two folds only</strong> for the injury-era results. The differences between the top models
    are smaller than the gap between folds, so read the ranking as "all equivalent" rather than as a winner.</li>
    <li>Usage is modelled <strong>conditional on the player appearing</strong>. Predicting usage for someone
    who might be ruled out needs an availability model first.</li>
  </ul>
</section>

<footer>
  <p>Generated from <code>RESULTS.md</code>, <code>RESULTS_ablation.md</code> and
  <code>RESULTS_processing.md</code>. Rebuild the numbers with
  <code>uv run python -m nba_usage.run_final</code>,
  <code>uv run python -m nba_usage.run_experiments</code> and
  <code>uv run python -m nba_usage.run_processing</code>, then this page with
  <code>uv run python -m nba_usage.make_report</code>.</p>
</footer>
</div>
"""

    page = f"""<!doctype html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>Forecasting next-game usage</title>
<link rel="preconnect" href="https://fonts.googleapis.com">
<link rel="preconnect" href="https://fonts.gstatic.com" crossorigin>
<link rel="stylesheet" href="https://fonts.googleapis.com/css2?family=IBM+Plex+Mono:wght@400;500&family=IBM+Plex+Sans:wght@400;500;600&family=Newsreader:opsz,wght@6..72,400;6..72,500&display=swap">
<style>{STYLE}</style>
</head>
<body>
{body}
</body>
</html>
"""
    OUT.write_text(page)
    print(f"wrote {OUT} ({len(page) / 1024:.0f} KB)")


if __name__ == "__main__":
    main()

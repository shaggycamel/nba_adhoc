# nba_adhoc

Ad hoc NBA analysis, aimed at predicting player usage (`usg_pct`) for a player's
next upcoming game using only information known before tip-off. See `CLAUDE.md`
for the project rules (Python + `uv`, `polars`, read-only database access).

## Team hierarchy model (`nba_hierarchy/`)

An algorithmic depth chart, built to run daily. For each team it ranks every
rostered player, assigns one of the five basketball positions, and reports
expected minutes and usage for the next game — accounting for who is injured
and who absorbs their minutes.

```sh
uv run python scripts/depth_chart.py BOS 2025-03-05   # one team's chart
uv run python scripts/eval_absorption.py              # reproduce the tables below
uv run pytest                                          # 55 tests
```

`pipeline.daily_hierarchy(date)` is the entry point. Every model inside it is
fitted only on games before the date it is asked about.

### The layers

| module | does | key output |
| --- | --- | --- |
| `data.py` | loads box scores, normalises the free-text DNP reason | `presence` |
| `roster.py` | rebuilds the rows the box score omits | roster-complete panel |
| `state.py` | 111 pre-game features from prior games only | trailing form |
| `position.py` | G/F/C learned, then split into five positions | `position`, `position_depth` |
| `availability.py` | calibrated probability the player takes the floor | `p_play` |
| `absorption.py` | who inherits an absent team-mate's minutes and usage | `expected_minutes`, `expected_usage` |

Each `nba_hierarchy/*.py` opens with the measurements behind its design
decisions, including the ones that argued against the obvious choice. The
`scripts/eval_*.py` reproduce every figure below.

### Why there is a panel

The box score only carries rows for players a team listed that night, so an
absent player usually leaves no trace: **92.7% of players the injury report
lists as Out have no box score row at all** (34,189 of 36,894 player-dates).
A model asked who absorbs an absent team-mate's minutes cannot see the absence
in that data.

`roster.py` reconstructs those rows from a membership interval per
(season, team, player), bounded by dated evidence, and takes the panel from
516,941 to 608,143 rows. It also fixes a quieter problem: a player who vanishes
for three months would otherwise keep a stale trailing average, as though no
time had passed.

### Results

Walk-forward throughout — each validation season is predicted by models trained
only on seasons that finished before it. Simple baselines first, as
`CLAUDE.md` requires.

**Position**, share of starters assigned the right G/F/C slot:

| method | accuracy |
| --- | --- |
| majority class | 0.4000 |
| the slot a player has started at most often before | 0.8529 |
| profile only | 0.8437 |
| profile + start history | 0.8804 |
| + matching the lineup to the 2G/2F/1C skeleton | **0.9504** |

The constraint supplies most of the gain. An unconstrained classifier does not
meaningfully beat knowing where a player has started before. This figure
assumes the five starters are known, so it measures slotting a lineup, not
predicting who starts.

**Availability**, whether the player takes the floor:

| model | log loss | Brier | AUC | ECE |
| --- | --- | --- | --- | --- |
| base rate | 0.6708 | 0.2389 | 0.5000 | 0.0111 |
| report status only | 0.4144 | 0.1336 | 0.8030 | 0.0549 |
| recent playing record only | 0.3787 | 0.1172 | 0.9018 | 0.0102 |
| both | 0.2548 | 0.0800 | 0.9555 | 0.0206 |
| both + isotonic calibration | 0.2581 | 0.0805 | 0.9544 | **0.0115** |

Injury earns its place — a third off the log loss. Note the other half of that
result: **the report alone is worse than the recent record alone.** It is a
strong complement, not a substitute.

**Minutes**, for players who played:

| model | MAE | RMSE | R² |
| --- | --- | --- | --- |
| season mean | 5.348 | 7.042 | 0.5430 |
| trailing mean | 5.163 | 6.753 | 0.5798 |
| mechanical allocation of the minute budget | 5.352 | 7.100 | 0.5361 |
| model, own form only | 5.052 | 6.535 | 0.6067 |
| model, own form + absorption | **4.762** | **6.158** | **0.6510** |

Absorption is worth 0.044 of R² overall, and 0.101 on the quartile of rows where
most of a team's minutes are in doubt — where the model reaches 0.491 against
0.390 for own form alone. The gain concentrating where the features apply is
what separates capturing the mechanism from adding model capacity.

The mechanical allocation is the instructive baseline. Sharing the budget out in
proportion to expected minutes matches a trailing mean overall, then collapses
to R² 0.199 exactly where absences are largest. A vacated minute does not spread
across a roster in proportion to who was already playing, which is why this
layer learns the allocation rather than assuming it.

Composed end to end, `expected_minutes` reaches **R² 0.787, MAE 4.0 minutes**
against actual minutes across every roster row, absentees counted as zero.

### What is weak

- **Usage barely beats a trailing mean** (R² 0.3421 against 0.3310, identical
  MAE to three decimals). Game-level usage is mostly noise once minutes are
  known. Since `usg_pct` is the wider project's target, this matters: build on
  the minutes and availability predictions, not the usage rate.
- **The five-position split is soft.** The coarse G/F/C slot is learned from
  real labels and holds 97.1% game to game. PG/SG and SF/PF are a convention
  imposed on top, because no source labels them; they hold 87.5%, and a quarter
  of the split decisions rest on a gap small enough to call arbitrary. Prefer
  the coarse slot where a hard label is needed.
- **Layers 3 and 4 are limited to 2021-22 onward**, where the injury report
  begins. Layers 1 and 2 use all of 2009-10 on. Extending absorption further
  back would need an availability model with no report in it.
- **2021-22's report is not comparable to later seasons** — 4.9% coverage
  against ~30% since, and an "Out" that season still played 8.2% of the time
  against 0.1-0.6% later. Excluding it from training was tested and changed
  nothing, so it is kept, but statistics cut by status should exclude it.
- A player waived and re-signed inside one season reads as a single long
  absence. `absence_run_full` flags implausible runs; 0.65% of panel rows sit
  in runs over 60 games.

### Leakage and reproducibility

Every feature is computed from games strictly before the row it sits on, and
that is asserted rather than assumed. `tests/` pins the arithmetic on synthetic
cases, recomputes real players' features from the definition by brute force, and
requires the daily path and the training build to agree to 1e-9 on a historical
date. Three cautions for anyone extending this:

- `absence_run_full` is deliberately forward-looking and is a diagnostic only.
  Features use `absent_streak_prior`. A test pins the difference.
- Absence is measured from `p_play`, never from who actually played. The injury
  report is known before tip-off; the outcome is not.
- **Sort on `data.ROW_KEY`, via `canonical_sort`, before any window or rank.**
  Polars' sort is not stable, so a tie in the key lets row order vary between
  runs, and with it every `.over("player_id")` feature and every
  `rank("ordinal")`. `(player, date, game)` is not unique: a player traded
  between two teams that then play each other has two rows for that one game.
  Until this was fixed the pipeline returned materially different answers on
  each run — one player's probability of playing moved from 0.058 to 0.076
  between two identical invocations. Two tests guard it, by shuffling the input
  and requiring identical output.


## Data

`data/gen.py` snapshots the source database to Parquet under `data/`, one
directory per schema:

- `data/nba/` — NBA.com-sourced tables (box scores, schedule, rosters,
  injuries, transactions).
- `data/statyx/` — Statyx-sourced tables (advanced stats, play types,
  contracts, usage shock, etc.).
- `data/util/` — views, not base tables (see the `table_type` filter in
  `data/gen.py`).

### `data/util/` — matching NBA and Statyx ids

The two sources number players differently: `player_id` in `data/nba/*` is an
NBA.com id, `player_id` in `data/statyx/*` is a Statyx id. The `util` objects
are the crosswalk between them, so any join across the two sources must go
through them rather than through player names.

- `player_id_map_vw.parquet` — one row per player, keyed by `player_key`, with
  the per-platform id and name side by side (`nba_id`/`nba_name`,
  `statyx_id`/`statyx_name`, plus `espn_*` and `yahoo_*`). This is the table to
  join on: map NBA `player_id` → `player_key` via `nba_id`, and `player_key` →
  Statyx `player_id` via `statyx_id`. Platform ids are nullable — a player
  present in one source may have no counterpart id in another.
- `active_player_vw.parquet` — long form of the same mapping, one row per
  (`season`, `platform`, `source_id`), carrying `player_key` and
  `conformed_name`. Use it when the match needs to be season-aware.
- `player_directory_vw.parquet` — every (`season`, `platform`, `source_id`,
  `source_name`) seen across the sources, matched or not.
- `unmatched_player_source_vw.parquet` — the directory rows with no
  `player_key`, i.e. players that failed to match. A non-empty file here means
  the crosswalk has gaps that will silently drop rows from cross-source joins.

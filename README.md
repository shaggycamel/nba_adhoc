# nba_adhoc

Ad hoc NBA analysis. See `CLAUDE.md` for the project rules (Python + `uv`,
`polars`, read-only database access).

Two threads:

- **Player usage** — predicting `usg_pct` for a player's next game using only
  information known before tip-off.
- **Injury duration** (`nba_injury/`) — predicting how many games an injury
  keeps a player out, and which variables drive it. Findings in
  [`reports/injury_duration.md`](reports/injury_duration.md).

## Injury duration

```sh
uv sync
uv run python scripts/01_build.py      # report + box score -> injury spells
uv run python scripts/02_describe.py   # Kaplan-Meier duration by injury type
uv run python scripts/03_models.py     # baselines vs hazard models, time splits
uv run python scripts/04_explain.py    # ablations, importance, partial dependence
uv run python scripts/05_forecast.py   # forecasts for the held-out season
```

`01_build.py` writes to `data/build/` (git-ignored); every later script reads
from there. Summary tables land in `reports/`.

The approach, in one paragraph: the box score does not record availability — a
player on a long absence is simply missing from it — so games missed have to
be counted from the per-game injury report instead. That gives player injury
*spells*, 22% of which never show a return because the season ends or the
player is traded or sent down. Those are the long ones, so they are kept as
right-censored rather than dropped: the Kaplan-Meier mean absence is 11.2
games against 3.6 for a censoring-blind average of observed spells. A
discrete-time hazard model over per-missed-game rows then uses every spell,
and the product of its per-game hazards gives expected games missed and
P(back within k games).

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

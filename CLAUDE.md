# nba_adhoc

Ad hoc NBA data analysis against the `nba` database on CockroachDB Cloud.

## Purpose
Predict **player usage** (`usg_pct` in `nba.player_box_score`) for a player's next upcoming game, using only information known before tip-off.
- Experiment with different data processing techniques (window lengths, weighting such as EWMA, minutes/role adjustments, handling DNPs, missing values and outliers, injury encodings) to find the best combination and the smallest subset of variables that forecasts usage well.
- Injury is probably a key variable: test it explicitly, both the player's own report status and teammates' absence (usage vacated by injured teammates). Check this with ablations and feature importance, not by assumption.
- Candidate signals: rolling trends, teammate availability/injuries, and player synergy (how a player's usage shifts with specific teammates on or off the floor).
- Try a wide range of algorithms, classical ML and neural networks: regularised linear (Ridge/ElasticNet), tree ensembles (random forest, extra trees, gradient boosting with LightGBM/XGBoost/CatBoost), plus PyTorch neural nets (MLP with player embeddings, and a sequence model over each player's recent games). Tune each fairly on the same validation folds.
- Compare variable sets, processing choices and algorithms on the same time-based splits, and report which combination wins and which variables drive usage.
- Use established packages (`polars`, `scikit-learn`, `lightgbm`). Evaluate with time-based splits only, never random shuffles, and compute every feature from prior games to avoid leakage.
- Always compare against simple baselines (season mean, last-N mean) before claiming a model helps.

## Stack
- Python only, managed with `uv`. Python version is pinned in `.python-version`.
- Add dependencies with `uv add`; run code with `uv run`. Don't use `pip` directly.
- Don't add R. Do analysis in Python.
- Use `polars` for data analysis. Avoid `pandas` unless a library requires it; convert at the boundary and say why.

## Database (CockroachDB Cloud)
- Cluster: `nba-data-mgmt` (id `11af18b7-ef5e-41b2-b3b7-5db438b1d403`), database `nba`, schema `nba`.
- **Read-only, always.** Use only `select_query`, `list_*`, `get_table_schema`, `explain_query`, `show_statement` and `show_running_queries`.
- Never use `create_database`, `create_table`, `insert_rows`, or any DDL/DML, even if a task seems to need it. Ask the user to run writes themselves.
- Use only schema `nba.nba`. Ignore schemas `util`, `anl` and `fty`: don't query them or use their tables as features.
- The MCP server caps each result at ~10 KB and this can't be raised. Pack rows server-side (gzip+base64, see `nba_usage/extract.py`) instead of paging raw rows.
- Prefer the `*_vw` views (e.g. `nba_injuries_vw`, `nba_player_box_score_vw`) over raw tables when they cover the question.
- Check the schema (`get_table_schema`) before writing queries. Add explicit `LIMIT`s and filters on large tables (`player_box_score` is ~590k rows).
- In cloud sessions the SQL port (26257) is blocked. Query through the Cloud MCP server at `https://cockroachlabs.cloud/mcp`.
- Locally, connect with `COCKROACH_URL`.

## Secrets
- Never print, log or commit `COCKROACH_URL`, API keys or any credential. `credentials.ini` is git-ignored; keep it that way.

## Git
- Work on the session branch; never push to `main`.
- Make small, focused commits with descriptive messages.
- Don't open a pull request unless asked.
- Don't commit raw data exports or large files.

# nba_adhoc

Ad hoc NBA data analysis against the `nba` database on CockroachDB Cloud.

## Stack
- Python only, managed with `uv`. Python version is pinned in `.python-version`.
- Add dependencies with `uv add`; run code with `uv run`. Don't use `pip` directly.
- Don't add R. Do analysis in Python.

## Database (CockroachDB Cloud)
- Cluster: `nba-data-mgmt` (id `11af18b7-ef5e-41b2-b3b7-5db438b1d403`), database `nba`, schema `nba`.
- **Read-only, always.** Use only `select_query`, `list_*`, `get_table_schema`, `explain_query`, `show_statement` and `show_running_queries`.
- Never use `create_database`, `create_table`, `insert_rows`, or any DDL/DML, even if a task seems to need it. Ask the user to run writes themselves.
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

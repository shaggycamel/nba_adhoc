"""Export the tables the usage model needs to local Parquet. Run on a machine that can reach port 26257.

    export COCKROACH_URL='postgresql://...'      # same URL used for psql
    uv run --group export python -m nba_usage.export_local

Read-only: the session is set read-only and only SELECT statements are issued.
Writes data/{player_box,schedule,injuries,player_info}.parquet (all git-ignored).
"""
import os
from pathlib import Path

import polars as pl
import psycopg

FIRST_GAME = 21500000  # 2015-16 regular season onward (incl. its playoffs)
DATA = Path(__file__).resolve().parent.parent / "data"

F = lambda c: f"{c}::float8 as {c}"  # noqa: E731  (cast DECIMAL/INT mixes to float for polars)

QUERIES = {
    "player_box": f"""
        select game_id::int8 as game_id, team_id::int8 as team_id, team_abbreviation,
               player_id::int8 as player_id, player_name, start_position, comment,
               {F('min')}, {F('fgm')}, {F('fga')}, {F('fg3_a')}, {F('fta')}, {F('tov')}, {F('ast')},
               {F('reb')}, {F('pts')}, {F('usg_pct')}, {F('ts_pct')}, {F('ast_pct')},
               {F('reb_pct')}, {F('tov_pct')}, {F('pace')}
        from nba.nba.player_box_score where game_id >= {FIRST_GAME}""",
    "schedule": f"""
        select game_id::int8 as game_id, game_date, season, season_type, team, opponent, home
        from nba.nba.league_game_schedule where game_id >= {FIRST_GAME}""",
    "injuries": """
        select game_date, game_id::int8 as game_id, team_slug, nba_id::int8 as nba_id,
               player_name, status, reason
        from nba.nba.injuries""",
    "player_info": """
        select season, player_id::int8 as player_id, birthdate, height_cm::float8 as height_cm,
               weight_kg::float8 as weight_kg, position, season_exp::int8 as season_exp,
               draft_year, draft_number
        from nba.nba.player_info""",
}

if __name__ == "__main__":
    DATA.mkdir(exist_ok=True)
    with psycopg.connect(os.environ["COCKROACH_URL"]) as conn:
        conn.read_only = True
        for name, sql in QUERIES.items():
            with conn.cursor() as cur:
                cur.execute(sql)
                cols = [d.name for d in cur.description]
                df = pl.DataFrame(cur.fetchall(), schema=cols, orient="row", infer_schema_length=None)
            df.write_parquet(DATA / f"{name}.parquet")
            print(f"{name}: {df.shape} -> data/{name}.parquet")

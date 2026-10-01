"""Read-only extract of the nba schema to local Parquet via the CockroachDB Cloud MCP server.

Run: uv run python -m nba_usage.extract [table ...]
Only SELECT statements are issued (see CLAUDE.md).
"""
import json
import os
import sys
from pathlib import Path

import httpx
import polars as pl

MCP_URL = "https://cockroachlabs.cloud/mcp"
CLUSTER_ID = "11af18b7-ef5e-41b2-b3b7-5db438b1d403"
DATA = Path(__file__).resolve().parent.parent / "data"

_client = httpx.Client(
    timeout=120,
    verify=os.environ.get("SSL_CERT_FILE") or os.environ.get("REQUESTS_CA_BUNDLE") or True,
)
_id = 0


def select(sql: str) -> list[dict]:
    """Run one read-only SELECT through the MCP select_query tool."""
    global _id
    _id += 1
    assert sql.lstrip().lower().startswith("select"), "read-only: SELECT only"
    body = {
        "jsonrpc": "2.0",
        "id": _id,
        "method": "tools/call",
        "params": {
            "name": "select_query",
            "arguments": {"cluster_id": CLUSTER_ID, "database": "nba", "query": sql},
        },
    }
    headers = {"Content-Type": "application/json", "Accept": "application/json, text/event-stream"}
    for attempt in range(4):
        try:
            r = _client.post(MCP_URL, json=body, headers=headers)
            r.raise_for_status()
            break
        except httpx.HTTPError:
            if attempt == 3:
                raise
    payload = next(json.loads(ln[6:]) for ln in r.text.splitlines() if ln.startswith("data: "))
    if "error" in payload:
        raise RuntimeError(payload["error"]["message"])
    return json.loads(payload["result"]["content"][0]["text"])["rows"]


def _enc(col: str, kind: str = "s") -> str:
    """SQL expression rendering one column as text for CSV packing."""
    if kind == "m":  # fraction -> per-mille integer
        return f"coalesce(round({col}*1000)::int::string,'')"
    return f"coalesce({col}::string,'')"


def packed(table_sql: str, key: str, cols: list[tuple[str, str]], names: list[str],
           lo, groups: int = 12, where: str = "true", key_is_str: bool = False) -> pl.DataFrame:
    """Fetch `table_sql` in key-ordered chunks of `groups` distinct key values.

    Each chunk is packed server-side into one gzip+base64 CSV cell to stay under the
    MCP ~10KB result cap. Chunk size halves automatically on overflow.
    """
    import base64
    import gzip
    import io

    frames: list[pl.DataFrame] = []
    last = lo
    total = 0
    while True:
        lit = f"'{last}'" if key_is_str else f"{last}"
        row = "||','||".join(_enc(c, k) for c, k in cols)
        sql = (
            f"select count(*) n, max({key})::string lastkey, "
            f"encode(compress(convert_to(string_agg({row}, chr(10)),'utf8'),'gzip'),'base64') b "
            f"from {table_sql} where {where} and {key} in "
            f"(select distinct {key} from {table_sql} where {where} and {key} > {lit} "
            f"order by {key} limit {groups})"
        )
        try:
            r = select(sql)[0]
        except RuntimeError as e:
            if groups > 1:
                groups = max(1, groups // 2)
                continue
            raise
        if not r["n"]:
            break
        csv = gzip.decompress(base64.b64decode(r["b"])).decode()
        frames.append(pl.read_csv(io.StringIO(csv), has_header=False, new_columns=names,
                                  infer_schema_length=None, null_values=[""]))
        total += r["n"]
        last = r["lastkey"] if key_is_str else int(r["lastkey"])
        print(f"  {total:,} rows (key<={r['lastkey']}, groups={groups})", flush=True)
        groups = min(groups * 2, 150) if len(r["b"]) < 5_000 else groups
    return pl.concat(frames, how="vertical_relaxed") if frames else pl.DataFrame()


def schedule() -> pl.DataFrame:
    cols = [(c, "s") for c in ("game_id", "game_date", "season", "season_type", "team", "opponent", "home")]
    return packed("nba.nba.league_game_schedule", "game_id", cols,
                  ["game_id", "game_date", "season", "season_type", "team", "opponent", "home"], FIRST_GAME, groups=30)


def player_box() -> pl.DataFrame:
    spec = [("game_id", "s"), ("team_id", "s"), ("team_abbreviation", "s"), ("player_id", "s"),
            ("(case when start_position is null or start_position='' then 0 else 1 end)", "s"),
            ("min", "s"), ("fga", "s"), ("fta", "s"), ("tov", "s"), ("ast", "s"), ("reb", "s"), ("pts", "s"),
            ("fg3_a", "s"), ("usg_pct", "m"), ("ts_pct", "m"), ("ast_pct", "m"), ("reb_pct", "m"),
            ("tov_pct", "m"), ("pace", "s")]
    names = ["game_id", "team_id", "team", "player_id", "starter", "min", "fga", "fta", "tov", "ast",
             "reb", "pts", "fg3a", "usg_pm", "ts_pm", "ast_pm", "reb_pm", "tov_pm", "pace"]
    return packed("nba.nba.player_box_score", "game_id", spec, names, FIRST_GAME, groups=12)


def injuries() -> pl.DataFrame:
    spec = [("game_date", "s"), ("team_slug", "s"), ("nba_id", "s"),
            ("replace(coalesce(player_name,''),',',' ')", "s"), ("replace(status,',',' ')", "s"),
            ("replace(replace(coalesce(reason,''),',',';'),chr(10),' ')", "s")]
    return packed("nba.nba.injuries", "game_date", spec,
                  ["game_date", "team", "player_id", "player_name", "status", "reason"], "1900-01-01",
                  groups=10, key_is_str=True)


def players() -> pl.DataFrame:
    """Distinct (player_id, name) for name-based injury matching. Keyed by player_id."""
    spec = [("player_id", "s"), ("replace(max(player_name),',',' ')", "s")]
    import base64, gzip, io
    out, last = [], 0
    while True:
        r = select(
            "select count(*) n, max(player_id) lastkey, encode(compress(convert_to(string_agg("
            "player_id::string||','||replace(nm,',',' '), chr(10)),'utf8'),'gzip'),'base64') b from ("
            "select player_id, max(player_name) nm from nba.nba.player_box_score "
            f"where game_id >= {FIRST_GAME} and player_id > {last} group by player_id "
            "order by player_id limit 400)")[0]
        if not r["n"]:
            break
        csv = gzip.decompress(base64.b64decode(r["b"])).decode()
        out.append(pl.read_csv(io.StringIO(csv), has_header=False, new_columns=["player_id", "player_name"]))
        last = int(r["lastkey"])
        print(f"  {sum(len(f) for f in out):,} players", flush=True)
    return pl.concat(out)


def player_info() -> pl.DataFrame:
    spec = [("season", "s"), ("player_id", "s"), ("birthdate", "s"), ("height_cm", "s"),
            ("weight_kg", "s"), ("replace(coalesce(position,''),',',';')", "s"),
            ("season_exp", "s"), ("draft_number", "s")]
    return packed("nba.nba.player_info", "season", spec,
                  ["season", "player_id", "birthdate", "height_cm", "weight_kg", "position",
                   "season_exp", "draft_number"], "0000", groups=2, key_is_str=True)


# earliest game_id to pull: 2017-18 regular season starts 21700001
FIRST_GAME = int(os.environ.get("FIRST_GAME", 21600000))

TABLES = {
    "schedule": schedule,
    "player_box": player_box,
    "injuries": injuries,
    "players": players,
    "player_info": player_info,
}

if __name__ == "__main__":
    DATA.mkdir(exist_ok=True)
    for name in sys.argv[1:] or TABLES:
        print(f"{name}:", flush=True)
        df = TABLES[name]()
        df.write_parquet(DATA / f"{name}.parquet")
        print(f"  -> data/{name}.parquet {df.shape}", flush=True)

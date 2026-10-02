"""Leak-free feature construction for next-game usage prediction (parquet only).

Every feature for a game is computed from games strictly before it. Rolling
statistics are computed over games the player actually played; reported
injury status is the pre-game report for that date.
"""
import re
import unicodedata
from pathlib import Path

import polars as pl

D = Path(__file__).resolve().parent.parent / "data"
CACHE = Path(__file__).resolve().parent.parent / ".cache"
MIN_MIN = 10  # target rows: appearances of at least this many minutes
SEV = {"Available": 1, "Probable": 2, "Questionable": 3, "Doubtful": 4, "Out": 5}


def norm_name(s: str | None) -> str | None:
    if s is None:
        return None
    s = unicodedata.normalize("NFKD", s).encode("ascii", "ignore").decode().lower()
    return re.sub(r"[^a-z ]", "", s).strip()


def load_games() -> tuple[pl.DataFrame, pl.DataFrame]:
    """Player-game rows (played or not) and the team-game table, NBA games only."""
    box = pl.read_parquet(D / "nba/player_box_score.parquet")
    sch = (pl.read_parquet(D / "nba/league_game_schedule.parquet")
           .filter(pl.col("season_type").is_in(["Regular Season", "Playoffs"]))
           .select("game_id", "game_date", "season", "season_type", pl.col("team").alias("team_abbreviation"),
                   "opponent", "home"))
    teams = pl.read_parquet(D / "nba/teams.parquet")
    nba_ids = teams["team_id"]
    box = (box.filter(pl.col("team_id").is_in(nba_ids))
           .join(sch, on=["game_id", "team_abbreviation"], how="inner")
           .unique(["game_id", "player_id"], keep="first"))
    tg = (box.select("game_id", "team_id", "game_date", "season", "season_type", "home", "opponent")
          .unique(["game_id", "team_id"]))
    # opponent team_id = the other team in the same game
    opp = tg.select("game_id", pl.col("team_id").alias("opp_team_id"))
    tg = tg.join(opp, on="game_id").filter(pl.col("team_id") != pl.col("opp_team_id"))
    return box, tg


def player_history(box: pl.DataFrame) -> pl.DataFrame:
    """Per played game: post-game rolling stats (suffix _post) plus a lagged copy (prior)."""
    p = (box.filter((pl.col("min") > 0) & pl.col("usg_pct").is_not_null())
         .sort("player_id", "game_date", "game_id")
         .with_columns(
             started=(pl.col("start_position").fill_null("") != "").cast(pl.Float64),
             fga36=pl.col("fga") / pl.col("min") * 36,
             fta36=pl.col("fta") / pl.col("min") * 36,
             tov36=pl.col("tov") / pl.col("min") * 36,
             fg3a36=pl.col("fg3_a") / pl.col("min") * 36,
             pts36=pl.col("pts") / pl.col("min") * 36,
             usgmin=pl.col("usg_pct") * pl.col("min")))
    g = "player_id"
    ex = []
    for n in (1, 3, 5, 10, 20):
        ex.append(pl.col("usg_pct").rolling_mean(n, min_samples=1).over(g).alias(f"usg_l{n}"))
    for n in (5, 10):
        ex.append(pl.col("min").rolling_mean(n, min_samples=1).over(g).alias(f"min_l{n}"))
        ex.append(pl.col("started").rolling_mean(n, min_samples=1).over(g).alias(f"start_l{n}"))
        ex.append((pl.col("usgmin").rolling_sum(n, min_samples=1).over(g)
                   / pl.col("min").rolling_sum(n, min_samples=1).over(g)).alias(f"usgw_l{n}"))
        ex.append(pl.col("usg_pct").rolling_std(n, min_samples=3).over(g).alias(f"usg_sd{n}"))
    for a in (0.1, 0.3):
        ex.append(pl.col("usg_pct").ewm_mean(alpha=a, adjust=False).over(g).alias(f"usg_ew{int(a*10)}"))
        ex.append(pl.col("min").ewm_mean(alpha=a, adjust=False).over(g).alias(f"min_ew{int(a*10)}"))
    for c in ("fga36", "fta36", "tov36", "fg3a36", "pts36", "ast_pct", "reb_pct", "ts_pct"):
        ex.append(pl.col(c).rolling_mean(10, min_samples=1).over(g).alias(f"{c}_l10"))
    n_in_group = pl.int_range(1, pl.len() + 1)
    ex += [
        (pl.col("usg_pct").cum_sum().over(g) / n_in_group.over(g)).alias("usg_career"),
        (pl.col("usg_pct").cum_sum().over([g, "season"]) / n_in_group.over([g, "season"])).alias("usg_season"),
        (pl.col("min").cum_sum().over([g, "season"]) / n_in_group.over([g, "season"])).alias("min_season"),
        n_in_group.over(g).alias("n_games"),
    ]
    return p.with_columns(ex)


POST = ["usg_l1", "usg_l3", "usg_l5", "usg_l10", "usg_l20", "min_l5", "min_l10", "start_l5", "start_l10",
        "usgw_l5", "usgw_l10", "usg_sd5", "usg_sd10", "usg_ew1", "usg_ew3", "min_ew1", "min_ew3",
        "fga36_l10", "fta36_l10", "tov36_l10", "fg3a36_l10", "pts36_l10", "ast_pct_l10", "reb_pct_l10",
        "ts_pct_l10", "usg_career", "usg_season", "min_season", "n_games"]


def add_prior(p: pl.DataFrame) -> pl.DataFrame:
    """Shift post-game stats by one played game to get pre-game (prior) features."""
    lag = [pl.col(c).shift(1).over("player_id").alias(c) for c in POST if c not in ("usg_season", "min_season")]
    lag += [pl.col(c).shift(1).over(["player_id", "season"]).alias(c) for c in ("usg_season", "min_season")]
    lag += [pl.col("game_date").shift(1).over("player_id").alias("prev_date"),
            pl.col("team_id").shift(1).over("player_id").alias("prev_team_id"),
            pl.col("min").shift(1).over("player_id").alias("min_last")]
    return p.with_columns(lag).with_columns((pl.col("n_games")).fill_null(0).alias("n_prior"))


def team_context(tg: pl.DataFrame) -> pl.DataFrame:
    """Lagged team pace / ratings and team-game counters."""
    tb = (pl.read_parquet(D / "nba/team_box_score.parquet")
          .select("game_id", "team_id", "pace", "off_rating", "def_rating"))
    t = (tg.join(tb, on=["game_id", "team_id"], how="left")
         .sort("team_id", "game_date", "game_id")
         .with_columns(pl.int_range(1, pl.len() + 1).over("team_id").alias("team_gcount")))
    roll = [pl.col(c).rolling_mean(10, min_samples=3).over("team_id").shift(1).over("team_id").alias(f"team_{c}_l10")
            for c in ("pace", "off_rating", "def_rating")]
    t = t.with_columns(roll).with_columns(
        (pl.col("game_date") - pl.col("game_date").shift(1).over("team_id")).dt.total_days().alias("team_rest"))
    return t


def injuries() -> pl.DataFrame:
    """Pre-game injury report: one row per (team_id, game_date, player_id) with max severity."""
    inj = pl.read_parquet(D / "nba/injuries.parquet")
    teams = pl.read_parquet(D / "nba/teams.parquet").with_columns(
        (pl.col("team_long") + " " + pl.col("team_name")).alias("team"))
    box = pl.read_parquet(D / "nba/player_box_score.parquet").select("player_id", "player_name", "game_id")
    names = (box.with_columns(pl.col("player_name").map_elements(norm_name, return_dtype=pl.String).alias("nn"))
             .sort("game_id").unique("nn", keep="last").select("nn", pl.col("player_id").alias("pid_name")))
    sev = (pl.when(pl.col("status").str.contains("Out")).then(5)
           .when(pl.col("status").str.contains("Doubtful")).then(4)
           .when(pl.col("status").str.contains("Questionable")).then(3)
           .when(pl.col("status").str.contains("Probable")).then(2)
           .when(pl.col("status").str.contains("Available")).then(1).otherwise(None))
    inj = (inj.with_columns(sev.alias("sev"),
                            pl.col("player_name").map_elements(norm_name, return_dtype=pl.String).alias("nn"))
           .join(names, on="nn", how="left").join(teams.select("team", "team_id"), on="team", how="left")
           .with_columns(pl.coalesce("nba_id", "pid_name").alias("player_id"))
           .filter(pl.col("sev").is_not_null() & pl.col("player_id").is_not_null() & pl.col("team_id").is_not_null())
           .group_by("team_id", "game_date", "player_id").agg(pl.col("sev").max()))
    return inj


def build() -> pl.DataFrame:
    box, tg = load_games()
    p = add_prior(player_history(box))
    t = team_context(tg)
    inj = injuries()
    first_inj_date = inj["game_date"].min()

    # team-game counts for games missed since the player's previous appearance
    tcount = t.select("team_id", "game_date", "team_gcount").sort("game_date")
    df = p.join(t.select("game_id", "team_id", "opp_team_id", "team_gcount", "team_rest",
                         "team_pace_l10", "team_off_rating_l10", "team_def_rating_l10"),
                on=["game_id", "team_id"], how="left")
    opp = t.select("game_id", pl.col("team_id").alias("opp_team_id"),
                   pl.col("team_pace_l10").alias("opp_pace_l10"),
                   pl.col("team_def_rating_l10").alias("opp_def_rating_l10"),
                   pl.col("team_off_rating_l10").alias("opp_off_rating_l10"))
    df = df.join(opp, on=["game_id", "opp_team_id"], how="left")
    prev = (df.select("team_id", "game_date", "prev_date").filter(pl.col("prev_date").is_not_null()).unique()
            .sort("prev_date")
            .join_asof(tcount.rename({"team_gcount": "c_prev", "game_date": "gd2"}), left_on="prev_date",
                       right_on="gd2", by="team_id", strategy="backward")
            .select("team_id", "game_date", "prev_date", "c_prev"))
    df = df.join(prev, on=["team_id", "game_date", "prev_date"], how="left")
    df = df.with_columns(
        (pl.col("team_gcount") - 1 - pl.col("c_prev")).alias("team_games_missed"),
        (pl.col("game_date") - pl.col("prev_date")).dt.total_days().alias("rest_days"),
        pl.col("home").cast(pl.Float64).alias("is_home"),
        (pl.col("season_type") == "Playoffs").cast(pl.Float64).alias("is_playoffs"))
    # own injury report
    df = df.join(inj.rename({"sev": "own_sev"}), on=["team_id", "game_date", "player_id"], how="left")
    df = df.with_columns(
        pl.when(pl.col("game_date") >= first_inj_date).then(pl.col("own_sev").fill_null(0)).otherwise(None)
        .alias("own_sev"))
    df = df.with_columns(pl.col("game_date").dt.year().alias("yr"))
    return df

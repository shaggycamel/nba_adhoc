"""Teammate-absence, rotation-hierarchy and pairwise with/without features.

Absence comes only from the pre-game injury report (never from who actually
played), so it is known before tip-off. All history is strictly prior.
"""
import polars as pl

from . import features as F

RATE = ["min_l10", "ast_pct_l10", "reb_pct_l10", "fg3a36_l10", "fta36_l10", "tov36_l10", "ts_pct_l10", "fga36_l10"]
FRESH_DAYS = 14
K_SHRINK = 8.0


def post_table(box: pl.DataFrame, t: pl.DataFrame) -> pl.DataFrame:
    p0 = F.player_history(box)
    p0 = p0.join(t.select("game_id", "team_id", "team_gcount"), on=["game_id", "team_id"], how="left")
    p0 = p0.with_columns((pl.col("usgw_l10") * pl.col("min_l10") / 48).alias("load"))
    return p0.select("player_id", "team_id", "game_id", "game_date", "team_gcount", "usg_pct", "min", "usgw_l10",
                     "load", *RATE)


def absent_table(inj: pl.DataFrame, p0: pl.DataFrame) -> pl.DataFrame:
    """Reported-unavailable (Out/Doubtful/Questionable) players with as-of-last-game load."""
    a = (inj.filter(pl.col("sev") >= 3).sort("game_date")
         .join_asof(p0.sort("game_date").select(
             "player_id", pl.col("game_date").alias("last_played"), "load", "min_l10", *RATE[1:]),
             left_on="game_date", right_on="last_played", by="player_id", strategy="backward",
             allow_exact_matches=False)
         .filter(pl.col("load").is_not_null())
         .with_columns((pl.col("game_date") - pl.col("last_played")).dt.total_days().alias("days_since"))
         .with_columns((pl.col("days_since") <= FRESH_DAYS).alias("fresh")))
    return a


def team_absence(df: pl.DataFrame, a: pl.DataFrame) -> pl.DataFrame:
    """Add team-level vacated load/minutes from absent teammates (self excluded)."""
    def s(sev_cond, fresh, col):
        m = sev_cond & (pl.col("fresh") == fresh) if fresh is not None else sev_cond
        return pl.when(m).then(pl.col(col)).otherwise(0.0)
    out, dbt, q = pl.col("sev") == 5, pl.col("sev") == 4, pl.col("sev") == 3
    agg = a.group_by("team_id", "game_date").agg(
        s(out, True, "load").sum().alias("t_vac_out_fresh"),
        s(out, False, "load").sum().alias("t_vac_out_stale"),
        s(out, True, "min_l10").sum().alias("t_vacmin_out_fresh"),
        s(out, None, "load").max().alias("t_vac_out_max"),
        out.cast(pl.Int32).filter(pl.col("fresh")).sum().alias("t_n_out_fresh"),
        out.cast(pl.Int32).filter(~pl.col("fresh")).sum().alias("t_n_out_stale"),
        s(dbt, None, "load").sum().alias("t_vac_doubt"),
        s(q, None, "load").sum().alias("t_vac_q"),
        q.cast(pl.Int32).sum().alias("t_n_q"))
    own = a.select("team_id", "game_date", "player_id", pl.col("sev").alias("o_sev"), pl.col("fresh").alias("o_fresh"),
                   pl.col("load").alias("o_load"), pl.col("min_l10").alias("o_min"))
    df = (df.join(agg, on=["team_id", "game_date"], how="left")
          .join(own, on=["team_id", "game_date", "player_id"], how="left"))
    inj_era = pl.col("own_sev").is_not_null()
    cols = ["t_vac_out_fresh", "t_vac_out_stale", "t_vacmin_out_fresh", "t_vac_out_max", "t_vac_doubt", "t_vac_q"]
    df = df.with_columns([pl.when(inj_era).then(pl.col(c).fill_null(0.0)).otherwise(None).alias(c) for c in cols]
                         + [pl.when(inj_era).then(pl.col(c).fill_null(0)).otherwise(None).alias(c)
                            for c in ("t_n_out_fresh", "t_n_out_stale", "t_n_q")])
    # remove the player's own contribution when he is on the report but plays anyway
    sub = lambda cond, col, o: pl.when(cond).then(pl.col(col) - pl.col(o)).otherwise(pl.col(col))
    df = df.with_columns(
        sub((pl.col("o_sev") == 5) & pl.col("o_fresh"), "t_vac_out_fresh", "o_load").alias("t_vac_out_fresh"),
        sub(pl.col("o_sev") == 4, "t_vac_doubt", "o_load").alias("t_vac_doubt"),
        sub(pl.col("o_sev") == 3, "t_vac_q", "o_load").alias("t_vac_q"),
        pl.when(pl.col("o_sev") == 3).then(pl.col("t_n_q") - 1).otherwise(pl.col("t_n_q")).alias("t_n_q"))
    return df.drop("o_sev", "o_fresh", "o_load", "o_min")


def rotation(p0: pl.DataFrame, a: pl.DataFrame, df: pl.DataFrame, t: pl.DataFrame) -> pl.DataFrame:
    """Healthy-rotation rank and absorption-weighted vacated usage/minutes."""
    r = p0.filter(pl.col("team_gcount").is_not_null()).select(
        "team_id", "team_gcount", "player_id", "load", "min_l10")
    parts = [r.with_columns((pl.col("team_gcount") + o).alias("tc"), pl.lit(o).alias("off")) for o in range(1, 11)]
    rot = (pl.concat(parts).sort("off").unique(["team_id", "tc", "player_id"], keep="first")
           .select("team_id", "tc", "player_id", "load", "min_l10"))
    dates = t.select("team_id", pl.col("team_gcount").alias("tc"), "game_date")
    rot = rot.join(dates, on=["team_id", "tc"], how="inner")
    absent_out = a.filter(pl.col("sev") >= 4).select("team_id", "game_date", "player_id").with_columns(
        pl.lit(True).alias("is_abs"))
    rot = (rot.join(absent_out, on=["team_id", "game_date", "player_id"], how="left")
           .with_columns(pl.col("is_abs").fill_null(False)))
    healthy = rot.filter(~pl.col("is_abs")).with_columns(
        pl.col("min_l10").rank("ordinal", descending=True).over("team_id", "tc").alias("healthy_rank"),
        pl.col("load").sum().over("team_id", "tc").alias("healthy_load_total"),
        pl.len().over("team_id", "tc").alias("healthy_size"))
    # reported-out players are excluded from `healthy`, so a player on the report who plays
    # anyway has no rank; keep him with his rank among the rest
    hr = healthy.select("team_id", "tc", "player_id", "healthy_rank", "healthy_load_total", "healthy_size",
                        pl.col("load").alias("own_load"))
    df = df.join(hr.rename({"tc": "team_gcount"}), on=["team_id", "team_gcount", "player_id"], how="left")
    inj_era = pl.col("own_sev").is_not_null()
    share = pl.col("own_load") / pl.col("healthy_load_total")
    df = df.with_columns(
        pl.when(inj_era).then(share).otherwise(None).alias("absorb_share"),
        pl.when(inj_era).then(share * pl.col("t_vac_out_fresh")).otherwise(None).alias("absorbed_load"),
        pl.when(inj_era).then(share * pl.col("t_vacmin_out_fresh")).otherwise(None).alias("absorbed_min"))
    return df


def pairwise(p0: pl.DataFrame, a: pl.DataFrame, df: pl.DataFrame, t: pl.DataFrame) -> pl.DataFrame:
    """Shrunk with/without usage effect of each currently-out teammate on this player."""
    base = p0.select("player_id", "team_id", "game_id", "game_date", "team_gcount", "usg_pct",
                     "usgw_l10", "min_l10").with_columns(pl.col("usg_pct").alias("y"))
    # deviation from the player's own prior form so drift is removed
    pr = df.select("player_id", "game_id", pl.col("usg_l10").alias("prior10"))
    base = base.join(pr, on=["player_id", "game_id"], how="left").with_columns(
        (pl.col("y") - pl.col("prior10")).alias("dev"))
    played = base.select("game_id", pl.col("player_id").alias("a_id")).with_columns(pl.lit(True).alias("a_played"))
    r = base.select("team_id", "team_gcount", pl.col("player_id").alias("a_id"), pl.col("min_l10").alias("a_min"))
    parts = [r.with_columns((pl.col("team_gcount") + o).alias("tc"), pl.lit(o).alias("off")) for o in range(1, 11)]
    rot = (pl.concat(parts).sort("off").unique(["team_id", "tc", "a_id"], keep="first")
           .filter(pl.col("a_min") >= 12).select("team_id", "tc", "a_id"))
    pairs = (base.filter(pl.col("dev").is_not_null() & (pl.col("min_l10") >= 12))
             .select("player_id", "team_id", "team_gcount", "game_id", "game_date", "dev")
             .join(rot.rename({"tc": "team_gcount"}), on=["team_id", "team_gcount"], how="inner")
             .filter(pl.col("a_id") != pl.col("player_id"))
             .join(played, on=["game_id", "a_id"], how="left")
             .with_columns(pl.col("a_played").fill_null(False))
             .sort("player_id", "a_id", "game_date", "game_id"))
    wo = pl.col("dev") * (~pl.col("a_played")).cast(pl.Float64)
    wi = pl.col("dev") * pl.col("a_played").cast(pl.Float64)
    g = ["player_id", "a_id"]
    pairs = pairs.with_columns(
        wo.cum_sum().over(g).shift(1).over(g).fill_null(0.0).alias("s_wo"),
        wi.cum_sum().over(g).shift(1).over(g).fill_null(0.0).alias("s_wi"),
        (~pl.col("a_played")).cast(pl.Float64).cum_sum().over(g).shift(1).over(g).fill_null(0.0).alias("n_wo"),
        pl.col("a_played").cast(pl.Float64).cum_sum().over(g).shift(1).over(g).fill_null(0.0).alias("n_wi"))
    pairs = pairs.with_columns(((pl.col("s_wo") / (pl.col("n_wo") + K_SHRINK))
                                - (pl.col("s_wi") / (pl.col("n_wi") + K_SHRINK))).alias("eff"))
    out = a.filter(pl.col("sev") >= 4).select("team_id", "game_date", pl.col("player_id").alias("a_id"))
    pe = (pairs.join(out, on=["team_id", "game_date", "a_id"], how="inner")
          .group_by("player_id", "game_id")
          .agg(pl.col("eff").sum().alias("pair_eff_sum"), pl.col("eff").min().alias("pair_eff_min"),
               pl.col("n_wo").sum().alias("pair_n_wo")))
    df = df.join(pe, on=["player_id", "game_id"], how="left")
    inj_era = pl.col("own_sev").is_not_null()
    df = df.with_columns([pl.when(inj_era).then(pl.col(c).fill_null(0.0)).otherwise(None).alias(c)
                          for c in ("pair_eff_sum", "pair_eff_min", "pair_n_wo")])
    return df


def build_all() -> tuple[pl.DataFrame, pl.DataFrame]:
    box, tg = F.load_games()
    t = F.team_context(tg)
    df = F.build()
    p0 = post_table(box, t)
    a = absent_table(F.injuries(), p0)
    df = team_absence(df, a)
    df = rotation(p0, a, df, t)
    df = pairwise(p0, a, df, t)
    return df, a

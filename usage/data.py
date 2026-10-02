"""Target frame, time-based folds, feature groups and fold-fitted role features."""
import numpy as np
import polars as pl
from sklearn.cluster import KMeans
from sklearn.decomposition import PCA
from sklearn.preprocessing import StandardScaler

from . import features as F

CACHE = F.CACHE
K_ROLES = 6
ROLE_IN = ["min_l10", "ast_pct_l10", "reb_pct_l10", "fg3a36_l10", "fta36_l10", "tov36_l10", "ts_pct_l10", "fga36_l10"]

GROUPS = {
    "core": ["usg_l1", "usg_l3", "usg_l5", "usg_l10", "usg_l20", "usgw_l5", "usgw_l10", "usg_sd5", "usg_sd10",
             "usg_ew1", "usg_ew3", "usg_season", "usg_career", "n_games"],
    "minutes": ["min_l5", "min_l10", "start_l5", "start_l10", "min_ew1", "min_ew3", "min_season", "min_last"],
    "rates": ["fga36_l10", "fta36_l10", "tov36_l10", "fg3a36_l10", "pts36_l10", "ast_pct_l10", "reb_pct_l10",
              "ts_pct_l10"],
    "context": ["is_home", "rest_days", "team_games_missed", "is_playoffs", "team_rest", "team_pace_l10",
                "team_off_rating_l10", "team_def_rating_l10", "opp_pace_l10", "opp_def_rating_l10",
                "opp_off_rating_l10"],
    "own_injury": ["own_sev"],
    "team_injury": ["t_vac_out_fresh", "t_vac_out_stale", "t_vacmin_out_fresh", "t_vac_out_max", "t_n_out_fresh",
                    "t_n_out_stale", "t_vac_doubt", "t_vac_q", "t_n_q"],
    "hierarchy": ["healthy_rank", "healthy_size", "healthy_load_total", "own_load", "absorb_share",
                  "absorbed_load", "absorbed_min"],
    "pairwise": ["pair_eff_sum", "pair_eff_min", "pair_n_wo"],
    "roles": [f"role_vac_c{i}" for i in range(K_ROLES)] + ["role_id", "role_vac_same", "role_vac_other",
                                                          "role_pc1", "role_pc2", "role_pc3"],
}
ORDER = list(GROUPS)

SEASONS = lambda df: sorted(df["season"].unique().to_list())
# (name, train seasons <, validation season); the last entry is the untouched holdout
FOLDS = [("val 2022-23", "2022-23", "2022-23"), ("val 2023-24", "2023-24", "2023-24"),
         ("val 2024-25", "2024-25", "2024-25"), ("holdout 2025-26", "2025-26", "2025-26")]


def load() -> tuple[pl.DataFrame, pl.DataFrame]:
    df = pl.read_parquet(CACHE / "full.parquet")
    a = pl.read_parquet(CACHE / "absent.parquet")
    df = df.filter((pl.col("min") >= F.MIN_MIN) & (pl.col("n_prior") >= 1)).with_columns(
        pl.col("rest_days").clip(upper_bound=30), pl.col("team_games_missed").clip(upper_bound=30))
    return df, a


def split(df: pl.DataFrame, train_before: str, val: str):
    tr = df.filter(pl.col("season") < train_before)
    va = df.filter(pl.col("season") == val)
    return tr, va


def fit_roles(tr: pl.DataFrame):
    X = tr.select(ROLE_IN).drop_nulls().to_numpy()
    sc = StandardScaler().fit(X)
    km = KMeans(K_ROLES, n_init=5, random_state=0).fit(sc.transform(X))
    pca = PCA(3, random_state=0).fit(sc.transform(X))
    return sc, km, pca


def add_roles(df: pl.DataFrame, a: pl.DataFrame, roles) -> pl.DataFrame:
    sc, km, pca = roles
    def prep(frame, cols):
        X = frame.select(cols).fill_null(strategy="mean").to_numpy()
        X = np.nan_to_num(X)
        return sc.transform(X)
    Z = prep(df, ROLE_IN)
    df = df.with_columns(pl.Series("role_id", km.predict(Z), dtype=pl.Float64),
                         *[pl.Series(f"role_pc{i+1}", pca.transform(Z)[:, i]) for i in range(3)])
    ab = a.filter((pl.col("sev") == 5) & pl.col("fresh"))
    Za = prep(ab, ROLE_IN)
    ab = ab.with_columns(pl.Series("arole", km.predict(Za)))
    piv = (ab.group_by("team_id", "game_date", "arole").agg(pl.col("load").sum())
           .pivot(on="arole", index=["team_id", "game_date"], values="load").fill_null(0.0))
    piv = piv.with_columns(pl.col("team_id").cast(pl.Int64)).rename({c: f"role_vac_c{c}" for c in piv.columns if c not in ("team_id", "game_date")})
    df = df.join(piv, on=["team_id", "game_date"], how="left")
    rc = [f"role_vac_c{i}" for i in range(K_ROLES)]
    for c in rc:
        if c not in df.columns:
            df = df.with_columns(pl.lit(None, dtype=pl.Float64).alias(c))
    inj = pl.col("own_sev").is_not_null()
    df = df.with_columns([pl.when(inj).then(pl.col(c).fill_null(0.0)).otherwise(None).alias(c) for c in rc])
    same = pl.sum_horizontal([pl.when(pl.col("role_id") == i).then(pl.col(f"role_vac_c{i}")).otherwise(0.0)
                              for i in range(K_ROLES)])
    tot = pl.sum_horizontal([pl.col(c) for c in rc])
    return df.with_columns(pl.when(inj).then(same).otherwise(None).alias("role_vac_same"),
                           pl.when(inj).then(tot - same).otherwise(None).alias("role_vac_other"))


def cols(groups: list[str]) -> list[str]:
    out = []
    for g in groups:
        out += GROUPS[g]
    return out


def metrics(y, p) -> dict:
    y, p = np.asarray(y), np.asarray(p)
    e = p - y
    return {"mae": float(np.abs(e).mean()), "rmse": float(np.sqrt((e ** 2).mean())),
            "r2": float(1 - (e ** 2).sum() / ((y - y.mean()) ** 2).sum()), "n": int(len(y))}

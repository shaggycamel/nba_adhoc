"""Role clusters from prior-game rate stats, and role-aware absence features.

Usage is deliberately excluded from the clustering inputs: the point is to
describe what kind of player this is from how they get their minutes, not to
smuggle the target into a feature. Clusters are refitted for each season on
earlier seasons only.

The clusters earn their keep, if at all, through role-aware absence: a wing's
usage should respond to a wing being out more than to a centre being out, and
that distinction is invisible to a single pooled `vacated_usg`.
"""

from __future__ import annotations

import numpy as np
import polars as pl
from sklearn.cluster import KMeans
from sklearn.mixture import GaussianMixture
from sklearn.preprocessing import StandardScaler

from .injuries import load_injuries

# Rate stats describing a role. No usage, by design.
ROLE_INPUTS = ["min", "fga", "fta", "ast_pct", "tov_pct", "ts_pct", "reb"]
N_ROLES = 6


def role_state(played: pl.DataFrame) -> pl.DataFrame:
    """Each player's rate profile as of the end of each game they played."""
    return (
        played.sort("player_id", "game_date")
        .with_columns(
            [
                pl.col(c).rolling_mean(10, min_samples=3).over("player_id").alias(f"rs_{c}")
                for c in ROLE_INPUTS
            ]
        )
        .select("player_id", "game_date", *[f"rs_{c}" for c in ROLE_INPUTS])
    )


def assign_roles(
    panel: pl.DataFrame,
    played: pl.DataFrame,
    n_roles: int = N_ROLES,
    method: str = "kmeans",
) -> pl.DataFrame:
    """Give every dressed player-game a role label from prior-game form.

    Walks seasons in order; the model for a season sees only earlier seasons.
    """
    cols = [f"rs_{c}" for c in ROLE_INPUTS]
    rows = (
        panel.select("game_id", "game_date", "season", "team_abbreviation", "player_id")
        .sort(["player_id", "game_date"])
        .join_asof(
            role_state(played).sort(["player_id", "game_date"]),
            on="game_date",
            by="player_id",
            strategy="backward",
            allow_exact_matches=False,
        )
    )

    seasons = sorted(rows["season"].unique().to_list())
    labelled = []
    for i, season in enumerate(seasons):
        cur = rows.filter(pl.col("season") == season)
        prior = rows.filter(pl.col("season").is_in(seasons[:i])).drop_nulls(cols)
        if i == 0 or prior.height < 1000:
            labelled.append(cur.with_columns(role=pl.lit(None, dtype=pl.Int32)))
            continue

        scaler = StandardScaler().fit(prior.select(cols).to_numpy())
        if method == "gmm":
            model = GaussianMixture(n_components=n_roles, random_state=0, covariance_type="diag")
        else:
            model = KMeans(n_clusters=n_roles, random_state=0, n_init=10)
        model.fit(scaler.transform(prior.select(cols).to_numpy()))

        ok = cur.drop_nulls(cols)
        if ok.is_empty():
            labelled.append(cur.with_columns(role=pl.lit(None, dtype=pl.Int32)))
            continue
        pred = model.predict(scaler.transform(ok.select(cols).to_numpy())).astype(np.int32)
        labelled.append(
            cur.join(
                ok.select("game_id", "player_id").with_columns(role=pl.Series("role", pred)),
                on=["game_id", "player_id"],
                how="left",
            )
        )

    return pl.concat(labelled).select("game_id", "player_id", "team_abbreviation", "role")


def role_absence_features(
    panel: pl.DataFrame, played: pl.DataFrame, roles: pl.DataFrame
) -> pl.DataFrame:
    """Vacated load split by whether the absent teammate shares the focal player's role.

    A ruled-out player has no row for the game they miss, so their team comes
    from the injury report and their role from the last game they did dress
    for.
    """
    from .injuries import player_state

    state = player_state(played)

    # Role as of the player's last dressed game, so an absent player still
    # carries one.
    role_history = (
        roles.join(panel.select("game_id", "player_id", "game_date"), on=["game_id", "player_id"], how="inner")
        .drop_nulls("role")
        .select("player_id", "game_date", "role")
        .sort(["player_id", "game_date"])
    )

    absent = (
        load_injuries()
        .filter(pl.col("is_out"))
        .select("game_id", "game_date", "team_abbreviation", "player_id")
        .unique()
        .sort(["player_id", "game_date"])
        .join_asof(state.sort(["player_id", "game_date"]), on="game_date", by="player_id",
                   strategy="backward", allow_exact_matches=False)
        .join_asof(role_history, on="game_date", by="player_id",
                   strategy="backward", allow_exact_matches=False)
        .with_columns(
            absent_load=pl.col("state_usg").fill_null(0.0) * pl.col("state_min").fill_null(0.0)
        )
        .rename({"role": "absent_role"})
    )

    by_role = absent.drop_nulls("absent_role").group_by(
        ["game_id", "team_abbreviation", "absent_role"]
    ).agg(role_load=pl.col("absent_load").sum())
    total = absent.group_by(["game_id", "team_abbreviation"]).agg(
        all_absent_load=pl.col("absent_load").sum()
    )

    focal = panel.select("game_id", "player_id", "team_abbreviation").join(
        roles, on=["game_id", "player_id", "team_abbreviation"], how="left"
    )
    return (
        focal.join(
            by_role,
            left_on=["game_id", "team_abbreviation", "role"],
            right_on=["game_id", "team_abbreviation", "absent_role"],
            how="left",
        )
        .join(total, on=["game_id", "team_abbreviation"], how="left")
        .with_columns(
            vacated_same_role=pl.col("role_load").fill_null(0.0),
            all_absent_load=pl.col("all_absent_load").fill_null(0.0),
        )
        .with_columns(vacated_other_role=pl.col("all_absent_load") - pl.col("vacated_same_role"))
        .select("game_id", "player_id", "team_abbreviation", "role",
                "vacated_same_role", "vacated_other_role")
    )


ROLE_COLS = ["role", "vacated_same_role", "vacated_other_role"]

"""Layer 4: who takes an absent teammate's minutes and usage.

Minutes are very nearly zero-sum. A team spends a fixed budget every night --
median 235 player-minutes, mean 236.5, standard deviation 7.3 almost all of
which is overtime -- so a thirty-minute player sitting out does not destroy
thirty minutes, it hands them to somebody. Minutes-weighted team usage is
tighter still, 0.1947 with a standard deviation of 0.0034. Both quantities are
therefore shares of a budget, and that is how this layer treats them.

The opening analysis of this project measured the effect directly: when a team's
leading minutes player sits, his team-mates gain 3.86 minutes and 1.44 usage
points on average. Two things in those numbers set the design.

The spread is twice the mean -- a standard deviation of 2.87 usage points
against a mean of 1.44, ranging from -6.2 to +10.0 -- so absorption is strongly
player-specific and a flat positional rule would be worthless.

Minutes and usage flow differently. Minutes run down the bench, rising
monotonically with rotation rank from +1.9 at rank 2 to roughly +6 at ranks
9-10, while usage concentrates at the top, +1.8 points at ranks 2-3 against
+0.6 at rank 8. They are modelled separately for that reason.

Absence is measured from expected availability, never from who actually played.
Before tip-off the injury report is known and the outcome is not, so every
absence feature here is built from layer 3's `p_play`. Using the real outcome
would both leak and skew the daily run against its own training data.

Pairwise with/without effects were the other candidate and are not used as the
primary estimator: only 330 player-seasons across seventeen years clear even a
five-game threshold on both sides, so most pairs have no usable sample. The
allocation features below pool over roles instead, which is what makes them
estimable.

Measured on 2024-25 and 2025-26, each trained only on earlier seasons:

    minutes, players who played      MAE    RMSE      R2
      season mean                   5.348   7.042  0.5430
      trailing mean                 5.163   6.753  0.5798
      allocation baseline           6.337   8.421  0.2015
      model, own form only          5.049   6.536  0.6065
      model, own + absorption       4.762   6.161  0.6507

    same, top quartile of expected vacated minutes
      model, own form only          5.665   7.350  0.3917
      model, own + absorption       5.247   6.728  0.4913

    usage, players who played
      season mean                   0.052   0.072  0.2898
      trailing mean                 0.050   0.069  0.3310
      model, own + absorption       0.050   0.069  0.3413

Absorption is worth 0.044 of R2 on minutes overall and 0.100 where a quarter or
more of the team's minutes are in doubt. The gain concentrating in exactly the
situations the features describe is the evidence that they capture the mechanism
rather than merely adding capacity.

Usage is the weak half, and the honest reading is that it barely beats a
trailing mean: 0.3413 against 0.3310, with the same mean absolute error to three
decimals. Game-level usage is mostly noise once minutes are known. Anything
built on top of this should lean on the minutes prediction.

Composed end to end -- p_play from layer 3 times E[minutes | played] -- expected
minutes reach R2 0.787 and a mean absolute error of 4.0 minutes against actual
minutes across every roster row, absentees counted as zero. That is well clear
of the conditional figure above, because knowing who turns up carries more of
the variance than knowing how long they stay on once they do.

Two things were tried and rejected on measurement. Rescaling each team's
expected minutes to its budget is a wash (mean absolute error 3.999 against
4.005, R2 0.7872 against 0.7880), so the zero-sum structure motivates the
features but is not worth enforcing on the output. And the mechanical allocation
baseline is poor, below even a trailing mean: a vacated minute does not spread
across a roster in proportion to who was already playing.
"""

from __future__ import annotations

import numpy as np
import polars as pl

TEAM_KEYS = ("game_id", "team_id")

# Targets, conditional on the player taking the floor. Expected minutes for the
# daily run are then p_play * E[minutes | played], which keeps layer 3's
# probability doing the work it is calibrated for.
MINUTES_TARGET = "min"
USAGE_TARGET = "usg_pct"

OWN_FEATURES = (
    "ewm_min_8",
    "ewm_min_20",
    "ewm_min_played_8",
    "ewm_usg_pct_8",
    "ewm_usg_pct_20",
    "play_rate_10",
    "depth_rank",
    "p_play",
    "days_rest",
    "team_games_missed",
    "career_games_prior",
    "season_games_prior",
)

# The absorption features proper. These are what the ablation tests.
ABSORPTION_FEATURES = (
    "expected_vacated_minutes",
    "expected_vacated_minutes_same_position",
    "expected_vacated_usage",
    "expected_vacated_usage_same_position",
    "expected_available_minutes",
    "available_rank",
    "rank_improvement",
    "expected_minute_share",
    "position_code",
    "team_size",
)

POSITION_CODES = {"PG": 0, "SG": 1, "SF": 2, "PF": 3, "C": 4}


def add_absorption_features(scored: pl.DataFrame) -> pl.DataFrame:
    """Team-level absence features, built from expected rather than actual absence.

    Requires `p_play` from layer 3 and `position` from layer 2 on every roster
    row of the team, absent players included -- which is the whole reason the
    panel exists.
    """
    absent_weight = 1.0 - pl.col("p_play")
    # Minutes the player gets WHEN HE PLAYS, not blended across absences.
    # `ewm_min_8` already counts an absence as zero minutes, so multiplying it
    # by p_play discounts availability twice: summed over a roster it lands 27%
    # under the team's actual minutes, against 3% for the played-only series
    # (and with half again the spread). E[min] = P(play) * E[min | play] needs
    # the second term on its own.
    baseline_minutes = pl.col("ewm_min_played_8").fill_null(0.0)
    # A player's own usage budget: his rate times the minutes he plays at it.
    baseline_usage = baseline_minutes * pl.col("ewm_usg_pct_8").fill_null(0.0)

    out = scored.with_columns(
        _expected_minutes=pl.col("p_play") * baseline_minutes,
        _vacated_minutes=absent_weight * baseline_minutes,
        _vacated_usage=absent_weight * baseline_usage,
        position_code=pl.col("position").replace_strict(POSITION_CODES).cast(pl.Int32),
    )

    team = list(TEAM_KEYS)
    team_pos = team + ["position"]
    out = out.with_columns(
        expected_available_minutes=pl.col("_expected_minutes").sum().over(team),
        # Minutes and usage expected to go unclaimed by their usual owner. The
        # player's own contribution is removed: his own likely absence is not
        # something he can absorb.
        expected_vacated_minutes=(
            pl.col("_vacated_minutes").sum().over(team) - pl.col("_vacated_minutes")
        ),
        expected_vacated_usage=(
            pl.col("_vacated_usage").sum().over(team) - pl.col("_vacated_usage")
        ),
        # Restricted to the player's own position: a missing centre frees
        # minutes a guard is unlikely to take.
        expected_vacated_minutes_same_position=(
            pl.col("_vacated_minutes").sum().over(team_pos) - pl.col("_vacated_minutes")
        ),
        expected_vacated_usage_same_position=(
            pl.col("_vacated_usage").sum().over(team_pos) - pl.col("_vacated_usage")
        ),
        # Standing among team-mates expected to be available, as against
        # standing on the full roster. A backup centre whose starter is out
        # climbs here while his `depth_rank` does not move, which is the
        # mechanism this layer exists to capture.
        available_rank=pl.col("_expected_minutes")
        .rank("ordinal", descending=True)
        .over(team)
        .cast(pl.Int32),
    )
    return out.with_columns(
        rank_improvement=pl.col("depth_rank") - pl.col("available_rank"),
        expected_minute_share=pl.when(pl.col("expected_available_minutes") > 0)
        .then(pl.col("_expected_minutes") / pl.col("expected_available_minutes"))
        .otherwise(0.0),
    ).drop("_expected_minutes", "_vacated_minutes", "_vacated_usage")


def allocation_baseline(
    df: pl.DataFrame, team_minutes: float, column: str = "alloc_minutes"
) -> pl.DataFrame:
    """Share out the team's minute budget in proportion to expected minutes.

    A mechanical absorption model with nothing learned in it: if a player is
    likely out, his expected minutes fall and everyone else's share rises to
    fill the budget. Any learned model has to beat this, not just the flat
    trailing average -- otherwise it is adding machinery for nothing.
    """
    share = pl.when(pl.col("expected_available_minutes") > 0).then(
        pl.col("expected_minute_share")
    ).otherwise(0.0)
    return df.with_columns((share * team_minutes).alias(column))


def _matrix(df: pl.DataFrame, features: tuple[str, ...]) -> np.ndarray:
    return df.select(features).to_numpy().astype(np.float64)


def fit_regressor(
    train: pl.DataFrame, features: tuple[str, ...], target: str, **kwargs
):
    """Gradient-boosted regressor on players who actually took the floor."""
    import lightgbm as lgb

    labelled = train.filter(pl.col(target).is_not_null() & pl.col("played"))
    params = {
        "objective": "l2",
        "learning_rate": 0.05,
        "num_leaves": 63,
        "min_child_samples": 100,
        "n_estimators": 500,
        "verbose": -1,
        "random_state": 0,
    }
    params.update(kwargs)
    model = lgb.LGBMRegressor(**params)
    model.fit(_matrix(labelled, features), labelled[target].to_numpy())
    return model


def predict(model, df: pl.DataFrame, features: tuple[str, ...], name: str) -> pl.DataFrame:
    return df.with_columns(pl.Series(name, model.predict(_matrix(df, features))))


def mae(p: np.ndarray, y: np.ndarray) -> float:
    return float(np.mean(np.abs(p - y)))


def rmse(p: np.ndarray, y: np.ndarray) -> float:
    return float(np.sqrt(np.mean((p - y) ** 2)))


def regression_score(p: np.ndarray, y: np.ndarray) -> dict[str, float]:
    denom = float(np.sum((y - y.mean()) ** 2))
    return {
        "mae": mae(p, y),
        "rmse": rmse(p, y),
        "r2": float(1 - np.sum((p - y) ** 2) / denom) if denom else float("nan"),
    }

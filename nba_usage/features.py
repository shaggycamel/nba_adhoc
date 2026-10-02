"""Prior-game features.

Every feature here is computed from games the player had already *played*
before the row's game date, then shifted by one, so nothing from the game
being predicted (or any later game) can reach the model. Features are built on
the played-only subframe because that is the universe the target is defined
on: usage is only observed when a player appears.
"""

from __future__ import annotations

import polars as pl

# Rate stats whose recent history plausibly describes a player's role.
ROLE_STATS = ["usg_pct", "min", "fga", "fta", "tov", "ast_pct", "tov_pct", "ts_pct"]

DEFAULT_WINDOWS = (3, 5, 10, 20)
DEFAULT_HALFLIVES = (3.0, 10.0)


def _lagged(col: str) -> pl.Expr:
    """The column as of the previous played game, per player."""
    return pl.col(col).shift(1).over("player_id")


def add_prior_features(
    played: pl.DataFrame,
    windows: tuple[int, ...] = DEFAULT_WINDOWS,
    halflives: tuple[float, ...] = DEFAULT_HALFLIVES,
    stats: list[str] | None = None,
) -> pl.DataFrame:
    """Attach rolling, EWMA and expanding history to played player-games.

    `played` must be sorted by player then game date and contain only rows
    where the player appeared.
    """
    stats = stats or ROLE_STATS
    out = played.sort("player_id", "game_date", "game_id")

    exprs: list[pl.Expr] = []
    for stat in stats:
        lag = _lagged(stat)
        for w in windows:
            exprs.append(lag.rolling_mean(w, min_samples=1).over("player_id").alias(f"{stat}_r{w}"))
        # Volatility matters for usage: a 20%-usage starter and a swing bench
        # player can share a mean.
        exprs.append(lag.rolling_std(windows[-1], min_samples=2).over("player_id").alias(f"{stat}_sd{windows[-1]}"))
        for hl in halflives:
            exprs.append(
                lag.ewm_mean(half_life=hl, ignore_nulls=True).over("player_id").alias(f"{stat}_ewm{hl:g}")
            )
        # Expanding means: season to date, then whole career.
        exprs.append(lag.cum_sum().over(["player_id", "season"]).alias(f"_{stat}_scum"))
        exprs.append(lag.cum_sum().over("player_id").alias(f"_{stat}_ccum"))

    out = out.with_columns(exprs)

    # Turn the running sums into means using the shifted appearance counts.
    means = []
    for stat in stats:
        means.append((pl.col(f"_{stat}_scum") / pl.col("season_game_n")).alias(f"{stat}_season"))
        means.append((pl.col(f"_{stat}_ccum") / pl.col("career_game_n")).alias(f"{stat}_career"))
    out = out.with_columns(means).drop([c for c in out.columns if c.startswith("_")])

    # Trend: is the player's recent usage drifting away from their season norm?
    out = out.with_columns(
        usg_trend_short=pl.col("usg_pct_r3") - pl.col("usg_pct_r10"),
        usg_vs_season=pl.col("usg_pct_r5") - pl.col("usg_pct_season"),
        is_season_debut=(pl.col("season_game_n") == 0),
        is_b2b=(pl.col("days_rest") == 1),
        days_rest_capped=pl.col("days_rest").clip(upper_bound=10),
    )
    return out


def feature_columns(df: pl.DataFrame, extra: list[str] | None = None) -> list[str]:
    """Model inputs: the engineered history plus a few known-before-tip facts."""
    engineered = [
        c
        for c in df.columns
        if any(c.startswith(f"{s}_") for s in ROLE_STATS)
        and not c.endswith("_pct")  # guard against re-adding the raw stats
    ]
    context = [
        "home",
        "days_rest_capped",
        "is_b2b",
        "season_game_n",
        "career_game_n",
        "usg_trend_short",
        "usg_vs_season",
    ]
    cols = [c for c in engineered + context if c in df.columns]
    cols += [c for c in (extra or []) if c in df.columns]
    # Deduplicate, keep order.
    seen: set[str] = set()
    return [c for c in cols if not (c in seen or seen.add(c))]

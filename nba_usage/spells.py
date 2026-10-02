"""Absence spells, and a discrete-time hazard model for how long they last.

The injury report says who is out tonight, so predicting *that* adds nothing
to a next-game forecast. What the report does not say is how serious the
absence is: a rested veteran and a torn calf are both "Out". That matters
because a coach fills a one-game gap differently from a six-week one, and
the replacement's minutes follow.

So the useful quantity is expected remaining absence, estimated on the night
it starts. This is a discrete-time hazard: for each game a player misses,
model the probability they are back for their team's next one. Logistic
regression over those rows handles right-censoring naturally, since a spell
still running at the end of the data simply contributes its observed
non-return rows and no return row.

Also here: how far into an absence the team already is (rotations settle),
and how recently a player came back (returning starters are eased in, so the
replacement does not revert the moment they are available).
"""

from __future__ import annotations

import numpy as np
import polars as pl

from .injuries import load_injuries
from .panel import DATA

# Spell causes behave differently: a G League assignment is a roster move,
# not an injury, and nearly a quarter of all "Out" rows are one.
CATEGORY = (
    pl.when(pl.col("reason").str.contains("(?i)g league")).then(pl.lit("g_league"))
    .when(pl.col("reason").str.contains("(?i)surgery|fracture|tear|torn|rupture|acl|achilles"))
    .then(pl.lit("severe"))
    .when(pl.col("reason").str.contains("(?i)injury|illness|sore|strain|sprain|contusion"))
    .then(pl.lit("injury"))
    .when(pl.col("reason").str.contains("(?i)personal|not with team|rest|suspension|coach"))
    .then(pl.lit("other"))
    .otherwise(pl.lit("unknown"))
)

BODY_PARTS = ["ankle", "knee", "hamstring", "calf", "back", "shoulder", "foot", "hip", "wrist", "groin", "illness"]


def team_game_index() -> pl.DataFrame:
    """Each team's games numbered within a season, so spells count games not days."""
    sched = (
        pl.read_parquet(DATA / "nba" / "league_game_schedule.parquet")
        .filter(pl.col("season_type") == "Regular Season")
        .select("game_id", "game_date", "season", pl.col("team").alias("team_abbreviation"))
        .unique()
    )
    return sched.sort("team_abbreviation", "season", "game_date").with_columns(
        tgi=pl.col("game_date").rank("ordinal").over(["team_abbreviation", "season"]).cast(pl.Int32)
    )


def build_spells() -> pl.DataFrame:
    """One row per missed game, tagged with its spell, cause and elapsed length."""
    out = load_injuries().filter(pl.col("is_out")).with_columns(cat=CATEGORY)
    out = out.with_columns(
        [
            pl.col("reason").fill_null("").str.contains(f"(?i){p}").alias(f"part_{p}")
            for p in BODY_PARTS
        ]
    )
    out = out.join(team_game_index(), on=["game_id", "team_abbreviation"], how="inner")

    out = out.sort("player_id", "season", "tgi").with_columns(
        gap=(pl.col("tgi") - pl.col("tgi").shift(1).over(["player_id", "season"])).fill_null(999)
    )
    out = out.with_columns(spell=(pl.col("gap") != 1).cum_sum().over(["player_id", "season"]))

    # games_elapsed counts games already missed *before* tonight, so it is
    # known at tip-off; spell_len is the full length, used only as a target.
    return out.with_columns(
        games_elapsed=pl.col("tgi").rank("ordinal").over(["player_id", "season", "spell"]).cast(pl.Int32) - 1,
        spell_len=pl.len().over(["player_id", "season", "spell"]).cast(pl.Int32),
    ).with_columns(
        # The hazard target: is this the last game of the spell?
        returns_next=(pl.col("games_elapsed") + 1 == pl.col("spell_len")).cast(pl.Int8)
    )


HAZARD_FEATURES = ["games_elapsed", "log_elapsed"] + [f"part_{p}" for p in BODY_PARTS]
CATS = ["g_league", "severe", "injury", "other", "unknown"]


# Standings terms: a team above the playoff line late in the season gets
# players back sooner. Measured, not assumed - adding these lifts hold-out
# AUC on every fold (0.669->0.675, 0.678->0.683, 0.741->0.750).
STANDING_TERMS = ["win_pct", "conf_rank", "season_frac", "tank_pressure", "push_pressure",
                  "contend_pressure", "locked_in"]


def _hazard_matrix(df: pl.DataFrame) -> np.ndarray:
    cols = [
        pl.col("games_elapsed").cast(pl.Float64),
        (pl.col("games_elapsed") + 1).log().alias("log_elapsed"),
        *[pl.col(f"part_{p}").cast(pl.Float64) for p in BODY_PARTS],
        *[(pl.col("cat") == c).cast(pl.Float64).alias(f"cat_{c}") for c in CATS],
        *[pl.col(c).cast(pl.Float64) for c in STANDING_TERMS if c in df.columns],
    ]
    return df.select(cols).to_numpy().astype(np.float64)


def expected_remaining(spells: pl.DataFrame, max_horizon: int = 30) -> pl.DataFrame:
    """Expected further games missed, per missed game, fitted on earlier seasons.

    Survival is read off the fitted hazard: the chance of still being out in
    k more games is the product of (1 - hazard) over the intervening games,
    and the expected remaining count is the sum of those survival terms.
    """
    from sklearn.linear_model import LogisticRegression

    seasons = sorted(spells["season"].unique().to_list())
    scored = []
    for i, season in enumerate(seasons):
        cur = spells.filter(pl.col("season") == season)
        prior = spells.filter(pl.col("season").is_in(seasons[:i]))
        # 2021-22's report matches too few games for consecutive absences to
        # join up, so every spell there looks like a single game and the
        # hazard target has no variation. Skip until the data supports a fit.
        usable = prior.height >= 500 and prior["returns_next"].n_unique() > 1
        if i == 0 or not usable:
            scored.append(cur.select("game_id", "player_id").with_columns(
                exp_remaining=pl.lit(None, dtype=pl.Float64),
                p_return_next=pl.lit(None, dtype=pl.Float64),
            ))
            continue

        model = LogisticRegression(max_iter=1000, C=1.0)
        model.fit(_hazard_matrix(prior), prior["returns_next"].to_numpy())

        # Hazard at the row's own elapsed count, then at each later one, to
        # build the survival curve forward.
        surv = np.ones(cur.height)
        total = np.zeros(cur.height)
        p_next = None
        for k in range(max_horizon):
            step = cur.with_columns(games_elapsed=pl.col("games_elapsed") + k)
            h = model.predict_proba(_hazard_matrix(step))[:, 1]
            if k == 0:
                p_next = h
            total += surv * (1 - h)   # still out after this game
            surv = surv * (1 - h)
        scored.append(
            cur.select("game_id", "player_id").with_columns(
                exp_remaining=pl.Series("exp_remaining", total),
                p_return_next=pl.Series("p_return_next", p_next),
            )
        )
    return pl.concat(scored)


def spell_features(panel: pl.DataFrame) -> pl.DataFrame:
    """Per player-game: how settled the team's absences are, and the ramp.

    For the focal player these describe the vacancy they are filling — how
    many games the team has already played short-handed, and how long the
    absentees are expected to stay out. `games_since_return` is the other
    side of it: a player just back from an absence is eased in, so the
    replacement does not lose their minutes the moment the starter is
    available again.
    """
    from .standings import standings_features

    spells = build_spells().join(
        standings_features(), on=["game_id", "team_abbreviation"], how="left"
    )
    est = expected_remaining(spells)
    absent = spells.join(est, on=["game_id", "player_id"], how="left").select(
        "game_id", "team_abbreviation", "player_id", "games_elapsed", "exp_remaining", "cat"
    )

    team = absent.group_by(["game_id", "team_abbreviation"]).agg(
        absent_exp_remaining_max=pl.col("exp_remaining").max(),
        absent_exp_remaining_sum=pl.col("exp_remaining").sum(),
        absent_games_elapsed_max=pl.col("games_elapsed").max(),
        absent_games_elapsed_mean=pl.col("games_elapsed").mean(),
        absent_severe=(pl.col("cat") == "severe").sum(),
        absent_fresh=(pl.col("games_elapsed") == 0).sum(),
    )

    # How long since the focal player's own last missed game, counted in
    # their team's games. Small values mean they are being eased back in.
    own = (
        absent.select("game_id", "player_id")
        .join(team_game_index().select("game_id", "team_abbreviation", "season", "tgi"), on="game_id", how="inner")
        .select("player_id", "season", "tgi")
        .unique()
        .with_columns(was_out=pl.lit(1, dtype=pl.Int8))
    )
    rows = (
        panel.select("game_id", "player_id", "team_abbreviation")
        .join(team_game_index().select("game_id", "team_abbreviation", "season", "tgi"), on=["game_id", "team_abbreviation"], how="left")
        .join(own, on=["player_id", "season", "tgi"], how="left")
        .sort("player_id", "season", "tgi")
    )
    rows = rows.with_columns(
        last_out_tgi=pl.when(pl.col("was_out") == 1).then(pl.col("tgi")).shift(1).forward_fill().over(["player_id", "season"])
    ).with_columns(
        games_since_return=(pl.col("tgi") - pl.col("last_out_tgi")).fill_null(99).clip(0, 99)
    )

    return (
        rows.select("game_id", "player_id", "team_abbreviation", "games_since_return")
        .join(team, on=["game_id", "team_abbreviation"], how="left")
        .with_columns(
            [pl.col(c).fill_null(0.0) for c in
             ["absent_exp_remaining_max", "absent_exp_remaining_sum", "absent_games_elapsed_max",
              "absent_games_elapsed_mean", "absent_severe", "absent_fresh"]]
        )
    )


SPELL_COLS = [
    "games_since_return",
    "absent_exp_remaining_max",
    "absent_exp_remaining_sum",
    "absent_games_elapsed_max",
    "absent_games_elapsed_mean",
    "absent_severe",
    "absent_fresh",
]

"""Opponent and own-team context, from each side's prior games.

Usage is a share of a team's own offence, so who the opponent is barely
moves it. Volume is different: points depend on how many possessions the
game has and how well the opponent defends, and rebounds depend on how much
the opponent leaves on the glass. These features are the ones the usage work
never needed.

Everything is a rolling mean over a team's previous games, shifted, so a
row only ever sees games played before its own.
"""

from __future__ import annotations

import polars as pl

from .panel import DATA

# Team-level signals worth carrying, from the team box score.
TEAM_STATS = ["pace", "poss", "off_rating", "def_rating", "dreb_pct", "oreb_pct", "pts", "reb", "ast"]
TEAM_WINDOW = 10


def team_form() -> pl.DataFrame:
    """Each team's rolling form as of before each of its games."""
    box = (
        pl.read_parquet(DATA / "nba" / "team_box_score.parquet")
        .select(["game_id", "team_abbreviation", *TEAM_STATS])
    )
    sched = (
        pl.read_parquet(DATA / "nba" / "league_game_schedule.parquet")
        .select("game_id", "game_date", pl.col("team").alias("team_abbreviation"))
    )
    joined = box.join(sched, on=["game_id", "team_abbreviation"], how="inner").sort(
        "team_abbreviation", "game_date", "game_id"
    )
    return joined.with_columns(
        [
            pl.col(s)
            .shift(1)
            .rolling_mean(TEAM_WINDOW, min_samples=2)
            .over("team_abbreviation")
            .alias(f"tm_{s}")
            for s in TEAM_STATS
        ]
    ).select("game_id", "team_abbreviation", "game_date", *[f"tm_{s}" for s in TEAM_STATS])


def add_context_features(frame: pl.DataFrame) -> pl.DataFrame:
    """Own-team and opponent form for every player-game."""
    form = team_form().drop("game_date")

    own = form.rename({c: c.replace("tm_", "own_") for c in form.columns if c.startswith("tm_")})
    opp = form.rename(
        {"team_abbreviation": "opponent", **{c: c.replace("tm_", "opp_") for c in form.columns if c.startswith("tm_")}}
    )

    out = (
        frame.join(own, on=["game_id", "team_abbreviation"], how="left")
        .join(opp, on=["game_id", "opponent"], how="left")
        .with_columns(
            # How fast the game itself should be: both teams contribute.
            game_pace=(pl.col("own_pace") + pl.col("opp_pace")) / 2,
            # A soft opponent lifts scoring; a good defensive rebounding
            # opponent suppresses the other side's offensive boards.
            opp_def_softness=pl.col("opp_def_rating"),
        )
    )
    return out


CONTEXT_COLS = (
    [f"own_{s}" for s in TEAM_STATS]
    + [f"opp_{s}" for s in TEAM_STATS]
    + ["game_pace", "opp_def_softness"]
)


def add_mismatch_features(frame: pl.DataFrame) -> pl.DataFrame:
    """How lopsided the game looks before it starts.

    A blowout empties the bench, and it empties both benches. For a backup
    filling in for an injured starter this is one of the few pre-game facts
    that moves their minutes a long way in either direction: a close game
    means the rotation tightens around whoever is left, a rout means the
    whole end of the bench plays. Direction matters too, since the losing
    side concedes garbage time earlier than the winning side grants it.
    """
    own_net = pl.col("own_off_rating") - pl.col("own_def_rating")
    opp_net = pl.col("opp_off_rating") - pl.col("opp_def_rating")
    return frame.with_columns(
        net_rating_diff=(own_net - opp_net),
        win_pct_diff=(pl.col("win_pct") - pl.col("opp_win_pct")),
    ).with_columns(
        # Size of the mismatch regardless of who is favoured: this is what
        # predicts garbage time existing at all.
        mismatch_size=pl.col("net_rating_diff").abs(),
        # Signed, with home advantage folded in, for who is likely ahead.
        expected_edge=pl.col("net_rating_diff") + pl.when(pl.col("home")).then(3.0).otherwise(-3.0),
    )


MISMATCH_COLS = ["net_rating_diff", "win_pct_diff", "mismatch_size", "expected_edge"]

"""Tests for the composed daily run.

This is the path that would run in production, and it is the only path where a
bug cannot be caught by comparing against a training build -- so the checks here
are about the composition itself: that the target date's rows carry the
information they are supposed to, and that the layers agree with each other.
"""

from __future__ import annotations

from datetime import date

import polars as pl
import pytest

from nba_hierarchy.position import POSITIONS
from nba_hierarchy.pipeline import daily_hierarchy

AS_OF = date(2025, 3, 5)


@pytest.fixture(scope="module")
def served() -> pl.DataFrame:
    return daily_hierarchy(AS_OF)


def test_one_row_per_player_on_one_team(served: pl.DataFrame):
    assert served.height == served["player_id"].n_unique()
    assert served.height == served.select("team_id", "player_id").unique().height


def test_the_target_dates_injury_report_is_attached(served: pl.DataFrame):
    """The anti-skew check that matters most for the daily run.

    Virtual rows are created after `build_panel` joined the report, so without
    an explicit re-join every player would look unreported -- and the model was
    trained in a world where roughly 30% of rows carry a status.
    """
    reported = served["report_status"].is_not_null().sum()
    assert reported > 20, f"only {reported} players carry a report on {AS_OF}"
    statuses = set(served["report_status"].drop_nulls().unique())
    assert statuses <= {"Out", "Doubtful", "Questionable", "Probable", "Available"}


def test_outputs_are_present_and_in_range(served: pl.DataFrame):
    for column in ("p_play", "expected_minutes", "expected_usage", "position",
                   "position_depth", "minutes_if_plays"):
        assert served[column].null_count() == 0, f"{column} has nulls"
    assert served["p_play"].min() >= 0.0 and served["p_play"].max() <= 1.0
    assert served["expected_minutes"].min() >= 0.0
    assert served["expected_minutes"].max() <= 48.0
    assert served["expected_usage"].min() >= 0.0 and served["expected_usage"].max() <= 1.0


def test_expected_minutes_is_availability_times_conditional_minutes(
    served: pl.DataFrame,
):
    """The composition must hold exactly, so the two factors stay interpretable."""
    recomputed = served["p_play"] * served["minutes_if_plays"].clip(0.0)
    assert (served["expected_minutes"] - recomputed).abs().max() == pytest.approx(
        0.0, abs=1e-9
    )


def test_every_team_gets_a_full_set_of_positions(served: pl.DataFrame):
    per_team = served.group_by("team_abbreviation").agg(
        *[(pl.col("position") == p).sum().alias(p) for p in POSITIONS],
        pl.len().alias("n"),
    )
    assert per_team.height >= 25, "expected most teams to have a roster"
    for p in POSITIONS:
        assert (per_team[p] >= 1).all(), f"a team has nobody at {p}"
    assert (per_team["n"] >= 10).all()


def test_doubtful_players_rank_below_their_own_depth(served: pl.DataFrame):
    """A player in doubt must lose standing among available team-mates.

    This is the whole point of separating depth rank from available rank: the
    chart has to demote an injured starter without forgetting he is a starter.
    """
    doubtful = served.filter(pl.col("report_status").is_in(["Out", "Doubtful"]))
    assert doubtful.height > 5
    # Averaged over such players, available rank is worse than full-roster rank.
    assert doubtful["rank_improvement"].mean() < 0
    assert doubtful["p_play"].mean() < served["p_play"].mean()


def test_calibration_split_is_order_independent():
    """The availability model's calibration slice must not move between runs.

    It is taken as the last fraction of training data by date. Millions of rows
    share a date and game id, so splitting on those alone cut arbitrarily among
    the ties and changed which rows calibrated the model -- which moved every
    probability it produced.
    """
    from nba_hierarchy.availability import (
        REPORT_FEATURES,
        STATE_FEATURES,
        add_report_features,
        fit_calibrated_play_model,
        injury_era,
    )
    from nba_hierarchy.data import load_player_games
    from nba_hierarchy.roster import build_panel
    from nba_hierarchy.state import add_pre_game_state

    data = injury_era(
        add_report_features(add_pre_game_state(build_panel(pg=load_player_games())))
    )
    train = data.filter(pl.col("season") < "2024-25")
    test = data.filter(pl.col("season") == "2024-25").head(4000)
    features = STATE_FEATURES + REPORT_FEATURES

    straight = fit_calibrated_play_model(train, features).predict(test)
    shuffled = fit_calibrated_play_model(
        train.sample(fraction=1.0, shuffle=True, seed=11), features
    ).predict(test)
    assert (straight == shuffled).all()

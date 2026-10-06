"""Tests for layer 3: the calibrated play probability.

The injury report is the one input that is legitimately known about the current
game before tip-off, which makes it the easiest place in the project to leak by
accident. These tests pin what may be read from the current row (the status
itself) against what may not (anything about how the absence turns out).
"""

from __future__ import annotations

from datetime import date, timedelta

import numpy as np
import polars as pl
import pytest

from nba_hierarchy.availability import (
    NO_REPORT,
    REPORT_FEATURES,
    REPORT_LEVELS,
    STATE_FEATURES,
    TARGET,
    add_report_features,
    expected_calibration_error,
    fit_calibrated_play_model,
    fit_play_model,
    injury_era,
    predict_play_probability,
    score,
)
from nba_hierarchy.config import INJURY_ERA_START
from nba_hierarchy.data import load_player_games
from nba_hierarchy.roster import build_panel
from nba_hierarchy.state import add_pre_game_state

CUTOFF = "2025-26"


@pytest.fixture(scope="module")
def data() -> pl.DataFrame:
    panel = build_panel(pg=load_player_games())
    return injury_era(add_report_features(add_pre_game_state(panel)))


# ------------------------------------------------------------ encoding


def _reports(statuses: list[str | None]) -> pl.DataFrame:
    n = len(statuses)
    return pl.DataFrame(
        {
            "player_id": [7] * n,
            "game_id": list(range(n)),
            "game_date": [date(2024, 1, 1) + timedelta(days=2 * i) for i in range(n)],
            "report_status": statuses,
        }
    )


def test_absence_of_a_report_is_its_own_state():
    """A missing report means "not mentioned", which is informative, not null."""
    out = add_report_features(_reports([None, "Out", None]))
    assert out["report_level"].null_count() == 0
    assert out["report_level"][0] == REPORT_LEVELS.index(NO_REPORT)
    assert out["report_level"][1] == REPORT_LEVELS.index("Out")


def test_out_streak_counts_only_prior_games():
    """The streak must not know the player stays out after this game."""
    out = add_report_features(_reports(["Out", "Out", "Out", None, "Out"]))
    assert out["report_out_streak"].to_list() == [0, 1, 2, 3, 0]


def test_prior_report_level_is_shifted():
    out = add_report_features(_reports([None, "Out", "Questionable"]))
    assert out["prior_report_level"][0] is None
    assert out["prior_report_level"][1] == REPORT_LEVELS.index(NO_REPORT)
    assert out["prior_report_level"][2] == REPORT_LEVELS.index("Out")
    assert out["report_is_new"].to_list() == [1, 1, 1]


def test_report_features_do_not_cross_players():
    a = _reports(["Out", "Out", "Out"])
    b = _reports([None, None, None]).with_columns(player_id=pl.lit(8, dtype=pl.Int64))
    out = add_report_features(pl.concat([a, b], how="vertical"))
    other = out.filter(pl.col("player_id") == 8)
    assert other["report_out_streak"].to_list() == [0, 0, 0]


def test_injury_era_filter_matches_the_report_start(data: pl.DataFrame):
    assert data["game_date"].min() >= INJURY_ERA_START


# ------------------------------------------------------------ the model


def test_report_beats_the_recent_record_ablation(data: pl.DataFrame):
    """The brief asks for injury to be tested, not assumed. This is that test."""
    train = data.filter(pl.col("season") < CUTOFF)
    test = data.filter(pl.col("season") == CUTOFF)
    y = test[TARGET].to_numpy().astype(int)

    m_state = fit_play_model(train, STATE_FEATURES)
    m_full = fit_play_model(train, STATE_FEATURES + REPORT_FEATURES)
    s_state = score(
        predict_play_probability(m_state, test, STATE_FEATURES)["p_play"].to_numpy(), y
    )
    s_full = score(
        predict_play_probability(
            m_full, test, STATE_FEATURES + REPORT_FEATURES
        )["p_play"].to_numpy(),
        y,
    )
    assert s_full["log_loss"] < s_state["log_loss"] * 0.85, (
        f"report should cut log loss well below state-only: "
        f"{s_full['log_loss']:.4f} vs {s_state['log_loss']:.4f}"
    )
    assert s_full["auc"] > 0.94


def test_isotonic_calibration_reduces_calibration_error(data: pl.DataFrame):
    """The level of the probability matters, not just its ranking."""
    features = STATE_FEATURES + REPORT_FEATURES
    train = data.filter(pl.col("season") < CUTOFF)
    test = data.filter(pl.col("season") == CUTOFF)
    y = test[TARGET].to_numpy().astype(int)

    raw = predict_play_probability(
        fit_play_model(train, features), test, features
    )["p_play"].to_numpy()
    calibrated = fit_calibrated_play_model(train, features).predict(test)

    assert expected_calibration_error(
        calibrated, y
    ) < expected_calibration_error(raw, y)
    # Ranking must survive the correction.
    assert score(calibrated, y)["auc"] > score(raw, y)["auc"] - 0.01


def test_probabilities_are_in_range_and_ordered_by_status(data: pl.DataFrame):
    features = STATE_FEATURES + REPORT_FEATURES
    train = data.filter(pl.col("season") < CUTOFF)
    test = data.filter(pl.col("season") == CUTOFF)
    p = fit_calibrated_play_model(train, features).predict(test)
    assert p.min() >= 0.0 and p.max() <= 1.0

    scored = test.with_columns(p_play=pl.Series(p))
    by_status = {
        r["report_level"]: r["mean"]
        for r in scored.group_by("report_level")
        .agg(pl.col("p_play").mean().alias("mean"))
        .to_dicts()
    }
    # Out must sit far below both Questionable and no report at all.
    assert by_status[REPORT_LEVELS.index("Out")] < 0.15
    assert by_status[REPORT_LEVELS.index("Out")] < by_status[
        REPORT_LEVELS.index("Questionable")
    ]
    assert by_status[REPORT_LEVELS.index("Out")] < by_status[
        REPORT_LEVELS.index(NO_REPORT)
    ]


def test_metrics_behave_on_known_inputs():
    y = np.array([1, 1, 0, 0])
    assert score(np.array([1.0, 1.0, 0.0, 0.0]), y)["auc"] == pytest.approx(1.0)
    assert score(np.array([0.5] * 4), y)["brier"] == pytest.approx(0.25)
    # A constant prediction at the base rate is perfectly calibrated.
    assert expected_calibration_error(np.array([0.5] * 4), y) == pytest.approx(0.0)

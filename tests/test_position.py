"""Tests for layer 2: the coarse slot model and the five-position split.

The two halves warrant different scrutiny. The G/F/C slot is supervised on real
labels, so it is held to beating its baselines on held-out seasons. The
PG/SG/SF/PF split has no labels anywhere, so it is held to structural
properties -- valid values, balanced counts, no leakage -- and its quality is
reported by the stability measurements in the module docstring.
"""

from __future__ import annotations

from datetime import date

import numpy as np
import polars as pl
import pytest

from nba_hierarchy.data import load_player_games
from nba_hierarchy.position import (
    HISTORY_FEATURES,
    POSITIONS,
    PROFILE_FEATURES,
    SLOT_QUOTA,
    SLOTS,
    add_start_history,
    assign_positions,
    constrained_slot_assignment,
    fit_slot_model,
    load_player_attributes,
    predict_slots,
    prepare,
)
from nba_hierarchy.roster import build_panel
from nba_hierarchy.state import add_pre_game_state

CUTOFF = "2024-25"


@pytest.fixture(scope="module")
def state() -> pl.DataFrame:
    return prepare(add_pre_game_state(build_panel(pg=load_player_games())))


@pytest.fixture(scope="module")
def scored(state: pl.DataFrame) -> pl.DataFrame:
    features = PROFILE_FEATURES + HISTORY_FEATURES
    train = state.filter(
        pl.col("start_position").is_in(SLOTS) & (pl.col("season") < CUTOFF)
    )
    model = fit_slot_model(train, features)
    test = state.filter((pl.col("season") == CUTOFF) & pl.col("played"))
    return assign_positions(predict_slots(model, test, features))


# ------------------------------------------------------------ inputs


def test_player_attributes_are_one_row_per_player():
    """A fan-out here would silently duplicate every row of the panel."""
    attrs = load_player_attributes()
    assert attrs.height == attrs["player_id"].n_unique()
    assert attrs["height_cm"].null_count() == 0


def test_prepare_does_not_change_row_count(state: pl.DataFrame):
    panel = build_panel(pg=load_player_games())
    assert state.height == panel.height


def test_start_history_excludes_the_current_game():
    """A player's first start must not count itself."""
    df = pl.DataFrame(
        {
            "player_id": [1] * 4,
            "game_id": [1, 2, 3, 4],
            "game_date": [date(2024, 1, d) for d in (1, 3, 5, 7)],
            "start_position": [None, "G", "G", "F"],
        }
    )
    out = add_start_history(df).sort("game_date")
    assert out["prior_starts_G"].to_list() == [0, 0, 1, 2]
    assert out["prior_starts_F"].to_list() == [0, 0, 0, 0]
    # Shares are undefined until the player has started at least once.
    assert out["prior_start_share_G"].to_list() == [None, None, 1.0, 1.0]


# ------------------------------------------------------------ coarse slot


def test_slot_model_beats_baselines_on_held_out_season(state: pl.DataFrame):
    """The learned slot must beat both the majority class and prior modal slot."""
    features = PROFILE_FEATURES + HISTORY_FEATURES
    starters = state.filter(pl.col("start_position").is_in(SLOTS))
    train = starters.filter(pl.col("season") < CUTOFF)
    test = starters.filter(pl.col("season") == CUTOFF)

    model = fit_slot_model(train, features)
    predicted = constrained_slot_assignment(predict_slots(model, test, features))
    accuracy = float((predicted["slot"] == predicted["start_position"]).mean())

    counts = np.column_stack([test[f"prior_starts_{s}"].to_numpy() for s in SLOTS])
    majority = train["start_position"].mode().first()
    modal = [
        SLOTS[i] if n > 0 else majority
        for i, n in zip(counts.argmax(axis=1), counts.sum(axis=1))
    ]
    modal_accuracy = float((pl.Series(modal) == test["start_position"]).mean())

    assert accuracy > 0.93, f"slot accuracy regressed to {accuracy:.4f}"
    assert accuracy > modal_accuracy + 0.05, (
        f"constrained model {accuracy:.4f} barely beats modal baseline "
        f"{modal_accuracy:.4f}; the 2G/2F/1C constraint should be worth ~10pp"
    )


def test_constrained_assignment_respects_the_lineup_quota(scored: pl.DataFrame):
    """Every five-player lineup must come out as exactly two G, two F, one C."""
    starters = scored.filter(pl.col("start_position").is_in(SLOTS))
    assigned = constrained_slot_assignment(starters)
    sizes = assigned.group_by(["game_id", "team_id"]).agg(
        pl.len().alias("n"),
        *[(pl.col("slot") == s).sum().alias(s) for s in SLOTS],
    )
    full = sizes.filter(pl.col("n") == 5)
    assert full.height > 1000
    for slot, quota in SLOT_QUOTA.items():
        assert (full[slot] == quota).all(), f"quota violated for {slot}"


def test_constrained_assignment_leaves_odd_lineups_alone():
    """Groups that are not five players keep their per-player argmax slot."""
    df = pl.DataFrame(
        {
            "game_id": [1, 1, 1],
            "team_id": [10, 10, 10],
            "p_G": [0.9, 0.8, 0.1],
            "p_F": [0.05, 0.1, 0.2],
            "p_C": [0.05, 0.1, 0.7],
            "slot": ["G", "G", "C"],
        }
    )
    assert constrained_slot_assignment(df)["slot"].to_list() == ["G", "G", "C"]


# ------------------------------------------------------------ five positions


def test_positions_are_valid_and_centres_are_unsplit(scored: pl.DataFrame):
    assert set(scored["position"].unique()) <= set(POSITIONS)
    centres = scored.filter(pl.col("slot") == "C")
    assert (centres["position"] == "C").all()
    assert scored["position"].null_count() == 0


def test_split_is_balanced_within_each_slot(scored: pl.DataFrame):
    """Neither side of a split may swallow the other."""
    per_team = scored.group_by(["game_id", "team_id"]).agg(
        *[(pl.col("position") == p).sum().alias(p) for p in POSITIONS],
        (pl.col("slot") == "G").sum().alias("n_g"),
        (pl.col("slot") == "F").sum().alias("n_f"),
    )
    # The lead position takes ceil(n/2), so the two sides differ by at most one.
    assert ((per_team["PG"] - per_team["SG"]).abs() <= 1).all()
    assert ((per_team["PF"] - per_team["SF"]).abs() <= 1).all()
    assert (per_team["PG"] + per_team["SG"] == per_team["n_g"]).all()
    assert (per_team["PF"] + per_team["SF"] == per_team["n_f"]).all()


def test_point_guards_outrank_shooting_guards_on_playmaking(scored: pl.DataFrame):
    """The split must follow its stated rule, team by team."""
    guards = scored.filter(pl.col("slot") == "G")
    worst_pg = guards.filter(pl.col("position") == "PG").group_by(
        ["game_id", "team_id"]
    ).agg(pl.col("ewm_ast_pct_20").fill_null(0.0).min().alias("lo"))
    best_sg = guards.filter(pl.col("position") == "SG").group_by(
        ["game_id", "team_id"]
    ).agg(pl.col("ewm_ast_pct_20").fill_null(0.0).max().alias("hi"))
    joined = worst_pg.join(best_sg, on=["game_id", "team_id"], how="inner")
    assert joined.height > 1000
    assert (joined["lo"] >= joined["hi"]).all()


def test_depth_is_dense_within_each_position(scored: pl.DataFrame):
    per = scored.group_by(["game_id", "team_id", "position"]).agg(
        pl.col("position_depth").min().alias("lo"),
        pl.col("position_depth").max().alias("hi"),
        pl.len().alias("n"),
    )
    assert (per["lo"] == 1).all()
    assert (per["hi"] == per["n"]).all()

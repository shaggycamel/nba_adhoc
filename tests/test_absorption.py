"""Tests for layer 4: minutes and usage absorption.

The absorption features are team-level, so the failure mode that matters is a
player absorbing his own absence, or a feature quietly reading the outcome of
the game it is predicting. Both are pinned here, alongside the ablation that
justifies the layer existing at all.
"""

from __future__ import annotations

import numpy as np
import polars as pl
import pytest

from nba_hierarchy.absorption import (
    ABSORPTION_FEATURES,
    MINUTES_TARGET,
    OWN_FEATURES,
    USAGE_TARGET,
    add_absorption_features,
    allocation_baseline,
    fit_regressor,
    predict,
    regression_score,
)
from nba_hierarchy.pipeline import layer4_frame

TEST_SEASON = "2025-26"


@pytest.fixture(scope="module")
def frame() -> pl.DataFrame:
    return layer4_frame()


def _team(p_play: list[float], minutes: list[float], positions: list[str]) -> pl.DataFrame:
    n = len(p_play)
    return pl.DataFrame(
        {
            "game_id": [1] * n,
            "team_id": [10] * n,
            "player_id": list(range(n)),
            "p_play": p_play,
            "ewm_min_played_8": minutes,
            "ewm_min_8": minutes,
            "ewm_usg_pct_8": [0.20] * n,
            "position": positions,
            "depth_rank": list(range(1, n + 1)),
        }
    )


# ------------------------------------------------------------ structure


def test_a_player_does_not_absorb_his_own_absence():
    """Vacated minutes must exclude the player they are a feature for."""
    out = add_absorption_features(
        _team([0.0, 1.0, 1.0], [30.0, 20.0, 10.0], ["PG", "SG", "C"])
    )
    # The likely-absent player vacates 30 minutes, which the other two see.
    assert out["expected_vacated_minutes"].to_list() == pytest.approx([0.0, 30.0, 30.0])


def test_same_position_vacancy_never_exceeds_total(frame: pl.DataFrame):
    assert (
        frame["expected_vacated_minutes_same_position"]
        <= frame["expected_vacated_minutes"] + 1e-9
    ).all()
    assert (
        frame["expected_vacated_usage_same_position"]
        <= frame["expected_vacated_usage"] + 1e-9
    ).all()


def test_vacancy_is_attributed_to_the_right_position():
    """A missing centre frees centre minutes, not guard minutes."""
    out = add_absorption_features(
        _team([0.0, 1.0, 1.0], [30.0, 25.0, 20.0], ["C", "C", "PG"])
    )
    by_pos = dict(zip(out["player_id"], out["expected_vacated_minutes_same_position"]))
    assert by_pos[1] == pytest.approx(30.0)  # the other centre
    assert by_pos[2] == pytest.approx(0.0)  # the guard sees none of it


def test_rank_improvement_is_a_redistribution(frame: pl.DataFrame):
    """Both ranks are permutations of a team's roster, so gains net to zero."""
    per_team = frame.group_by(["game_id", "team_id"]).agg(
        pl.col("rank_improvement").sum().alias("net"),
        pl.col("available_rank").min().alias("lo"),
        pl.col("available_rank").max().alias("hi"),
        pl.len().alias("n"),
    )
    assert (per_team["net"] == 0).all()
    assert (per_team["lo"] == 1).all()
    assert (per_team["hi"] == per_team["n"]).all()


def test_an_absent_starter_promotes_his_backup():
    """The mechanism the layer exists for: the backup climbs, the starter falls."""
    healthy = add_absorption_features(
        _team([1.0, 1.0, 1.0], [32.0, 16.0, 10.0], ["C", "C", "PG"])
    )
    injured = add_absorption_features(
        _team([0.05, 1.0, 1.0], [32.0, 16.0, 10.0], ["C", "C", "PG"])
    )
    backup_before = healthy.filter(pl.col("player_id") == 1)["available_rank"][0]
    backup_after = injured.filter(pl.col("player_id") == 1)["available_rank"][0]
    assert backup_after < backup_before
    assert injured.filter(pl.col("player_id") == 1)["rank_improvement"][0] == 1
    # The starter's own depth rank is untouched; only his availability changed.
    assert injured.filter(pl.col("player_id") == 0)["depth_rank"][0] == 1


def test_allocation_baseline_spends_the_whole_budget():
    out = allocation_baseline(
        add_absorption_features(_team([1.0, 0.5, 1.0], [30.0, 20.0, 10.0], ["PG", "SG", "C"])),
        team_minutes=236.0,
    )
    assert out["alloc_minutes"].sum() == pytest.approx(236.0)


def test_absorption_features_exclude_the_current_outcome():
    forbidden = {"min", "usg_pct", "played", "absence_run_full", "start_position"}
    assert not (set(OWN_FEATURES) | set(ABSORPTION_FEATURES)) & forbidden


def test_frame_is_injury_era_with_out_of_fold_probability(frame: pl.DataFrame):
    """The first injury-era season has no earlier season to train on, so it goes."""
    seasons = sorted(frame["season"].unique().to_list())
    assert "2021-22" not in seasons
    assert frame["p_play"].null_count() == 0
    assert frame["p_play"].min() >= 0.0 and frame["p_play"].max() <= 1.0


# ------------------------------------------------------------ the ablation


def test_absorption_beats_own_form_on_minutes(frame: pl.DataFrame):
    """The layer has to earn its place against a player's own recent form."""
    combined = OWN_FEATURES + ABSORPTION_FEATURES
    train = frame.filter((pl.col("season") < TEST_SEASON) & pl.col("played"))
    test = frame.filter((pl.col("season") == TEST_SEASON) & pl.col("played"))

    own = predict(
        fit_regressor(train, OWN_FEATURES, MINUTES_TARGET), test, OWN_FEATURES, "p"
    )["p"].to_numpy()
    allf = predict(
        fit_regressor(train, combined, MINUTES_TARGET), test, combined, "p"
    )["p"].to_numpy()
    y = test[MINUTES_TARGET].to_numpy()

    s_own, s_all = regression_score(own, y), regression_score(allf, y)
    assert s_all["mae"] < s_own["mae"], f"{s_all['mae']:.3f} vs {s_own['mae']:.3f}"
    assert s_all["r2"] > s_own["r2"] + 0.02
    # And beat the trailing mean, per the brief's baseline requirement.
    trailing = test["ewm_min_played_8"].fill_null(float(train[MINUTES_TARGET].mean())).to_numpy()
    assert s_all["mae"] < regression_score(trailing, y)["mae"]


def test_absorption_gain_is_larger_where_more_is_vacated(frame: pl.DataFrame):
    """If the features work, they should help most when there is most to absorb.

    A gain that did not concentrate where absences are large would suggest the
    extra columns were only adding model capacity.
    """
    combined = OWN_FEATURES + ABSORPTION_FEATURES
    train = frame.filter((pl.col("season") < TEST_SEASON) & pl.col("played"))
    test = frame.filter((pl.col("season") == TEST_SEASON) & pl.col("played"))
    m_own = fit_regressor(train, OWN_FEATURES, MINUTES_TARGET)
    m_all = fit_regressor(train, combined, MINUTES_TARGET)
    scored = predict(m_all, predict(m_own, test, OWN_FEATURES, "p_own"), combined, "p_all")

    def gain(df: pl.DataFrame) -> float:
        y = df[MINUTES_TARGET].to_numpy()
        return regression_score(df["p_all"].to_numpy(), y)["r2"] - regression_score(
            df["p_own"].to_numpy(), y
        )["r2"]

    cut = scored["expected_vacated_minutes"].quantile(0.75)
    high = gain(scored.filter(pl.col("expected_vacated_minutes") >= cut))
    low = gain(scored.filter(pl.col("expected_vacated_minutes") < cut))
    assert high > low, f"gain should concentrate in absences: {high:.4f} vs {low:.4f}"


def test_usage_absorption_is_measured_not_assumed(frame: pl.DataFrame):
    """Usage is the weak half of this layer, and the test says so.

    The gain over a trailing mean is real but small, so this asserts only that
    it is not negative. Anyone tightening this bound should check the evaluation
    script first rather than assume headroom exists.
    """
    combined = OWN_FEATURES + ABSORPTION_FEATURES
    train = frame.filter((pl.col("season") < TEST_SEASON) & pl.col("played"))
    test = frame.filter((pl.col("season") == TEST_SEASON) & pl.col("played"))
    y = test[USAGE_TARGET].to_numpy()

    model = predict(
        fit_regressor(train, combined, USAGE_TARGET), test, combined, "p"
    )["p"].to_numpy()
    trailing = test["ewm_usg_pct_8"].fill_null(float(train[USAGE_TARGET].mean())).to_numpy()
    assert regression_score(model, y)["r2"] >= regression_score(trailing, y)["r2"]

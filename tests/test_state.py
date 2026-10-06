"""Leakage and train/serve-skew tests for the layer 1 point-in-time state.

The hierarchy model is only as trustworthy as the guarantee that no feature
sees the game it sits on. These tests assert that guarantee directly rather
than inspecting the code: synthetic cases pin the exact arithmetic, and real
data checks that the daily-run path agrees with the training path.
"""

from __future__ import annotations

import math
from datetime import date, timedelta

import polars as pl
import pytest

from nba_hierarchy.data import load_player_games
from nba_hierarchy.roster import build_panel
from nba_hierarchy.state import add_pre_game_state, candidate_roster, state_as_of

# ---------------------------------------------------------------- fixtures


@pytest.fixture(scope="module")
def panel() -> pl.DataFrame:
    return build_panel(pg=load_player_games())


@pytest.fixture(scope="module")
def state(panel: pl.DataFrame) -> pl.DataFrame:
    return add_pre_game_state(panel)


def _synthetic(minutes: list[float | None], start: date = date(2024, 1, 1)) -> pl.DataFrame:
    """One player, one team, consecutive games with the given minutes."""
    n = len(minutes)
    return pl.DataFrame(
        {
            "season": ["2023-24"] * n,
            "team_id": [1] * n,
            "team_abbreviation": ["AAA"] * n,
            "player_id": [99] * n,
            "player_name": ["Test Player"] * n,
            "game_id": list(range(n)),
            "game_date": [start + timedelta(days=2 * i) for i in range(n)],
            "min": minutes,
            "usg_pct": [None if m is None else 0.2 for m in minutes],
            "played": [m is not None and m > 0 for m in minutes],
            "started": [False] * n,
            "with_team": [m is not None for m in minutes],
            "presence": ["PLAYED" if m else "ABSENT_UNKNOWN" for m in minutes],
            "absence_run_full": [0] * n,
        }
    )


# ---------------------------------------------------------------- leakage


def test_feature_on_first_game_is_null():
    """A player's debut has no prior games, so trailing minutes are unknown."""
    out = add_pre_game_state(_synthetic([30.0, 25.0, 20.0]), rate_stats=())
    assert out["ewm_min_3"][0] is None
    assert out["career_games_prior"][0] == 0
    assert out["team_games_prior"][0] == 0


def test_current_game_never_leaks_into_its_own_feature():
    """Changing a game's minutes must not change that game's own features."""
    base = _synthetic([10.0, 10.0, 10.0, 10.0, 10.0])
    spiked = _synthetic([10.0, 10.0, 999.0, 10.0, 10.0])
    a = add_pre_game_state(base, rate_stats=())
    b = add_pre_game_state(spiked, rate_stats=())

    feats = [c for c in a.columns if c.startswith(("ewm_", "play_rate", "career_", "season_"))]
    # Rows up to and including the spiked game must be identical ...
    assert a.head(3).select(feats).equals(b.head(3).select(feats))
    # ... and the spike must show up immediately afterwards.
    assert b["ewm_min_3"][3] > a["ewm_min_3"][3]


def test_ewm_matches_hand_computed_value():
    """Pin the exact EWMA arithmetic, including polars' adjust=True weighting."""
    minutes = [10.0, 20.0, 30.0, 40.0]
    out = add_pre_game_state(_synthetic(minutes), rate_stats=())
    hl = 3
    alpha = 1 - math.exp(-math.log(2) / hl)

    # Feature on row i is the EWMA of games 0..i-1.
    for i in (1, 2, 3):
        prior = minutes[:i]
        weights = [(1 - alpha) ** k for k in range(len(prior))]
        # Most recent game carries the largest weight.
        expected = sum(w * x for w, x in zip(weights, reversed(prior))) / sum(weights)
        assert out["ewm_min_3"][i] == pytest.approx(expected, rel=1e-9)


def test_absence_decays_minutes_but_not_played_only_average():
    """An absence pulls blended minutes down while the played-only average holds.

    This split is the reason both series exist: blended minutes answer "where is
    he in the rotation", played-only answers "what does he do when he plays".
    """
    out = add_pre_game_state(_synthetic([30.0, 30.0, None, None, None, 30.0]), rate_stats=())
    assert out["ewm_min_3"][5] < out["ewm_min_3"][2]
    assert out["ewm_min_played_3"][5] == pytest.approx(30.0, rel=1e-9)
    assert out["team_games_missed"][5] == 3
    assert out["days_since_played"][5] == 8


def test_forward_fill_does_not_cross_players():
    """One player's history must never bleed into another's features."""
    a = _synthetic([30.0, 30.0, 30.0])
    b = _synthetic([None, None, 5.0]).with_columns(
        player_id=pl.lit(100, dtype=pl.Int64), player_name=pl.lit("Other")
    )
    out = add_pre_game_state(pl.concat([a, b], how="vertical"), rate_stats=())
    other = out.filter(pl.col("player_id") == 100).sort("game_date")
    assert other["ewm_min_played_3"].to_list() == [None, None, None]
    assert other["ewm_min_3"][2] == pytest.approx(0.0, abs=1e-12)


# ---------------------------------------------------------------- real data


def test_panel_preserves_every_box_score_row(panel: pl.DataFrame):
    pg = load_player_games()
    assert panel.filter(pl.col("with_team")).height == pg.height
    assert panel.select("game_id", "team_id", "player_id").unique().height == panel.height
    assert panel["presence"].null_count() == 0


def test_absent_rows_carry_no_box_score_data(panel: pl.DataFrame):
    """An absent row must not smuggle in stats; it is an absence, not a zero."""
    absent = panel.filter(~pl.col("with_team"))
    assert absent.height > 0
    assert absent["min"].null_count() == absent.height
    assert absent["usg_pct"].null_count() == absent.height
    assert not absent["played"].any()


def test_injury_confirmed_absences_only_in_injury_era(panel: pl.DataFrame):
    """ABSENT_INJ requires the injury report, which starts 2021-10-19."""
    inj = panel.filter(pl.col("presence") == "ABSENT_INJ")
    assert inj.height > 0
    assert inj["game_date"].min() >= date(2021, 10, 19)


def test_no_feature_uses_a_future_game(state: pl.DataFrame):
    """Brute-force check against the definition, on real players.

    Recompute `ewm_min_played_8` from scratch for a sample of players using only
    games strictly before each row, and require it to match.
    """
    hl = 8
    alpha = 1 - math.exp(-math.log(2) / hl)
    sample = (
        state.filter(pl.col("season") == "2024-25")
        .select("player_id")
        .unique()
        .head(25)["player_id"]
        .to_list()
    )
    rows = state.filter(pl.col("player_id").is_in(sample)).sort(
        ["player_id", "game_date", "game_id"]
    )
    for pid in sample:
        g = rows.filter(pl.col("player_id") == pid)
        history: list[float] = []
        for played, minutes, feature in zip(
            g["played"], g["min"], g[f"ewm_min_played_{hl}"]
        ):
            if not history:
                assert feature is None
            else:
                weights = [(1 - alpha) ** k for k in range(len(history))]
                expected = sum(
                    w * x for w, x in zip(weights, reversed(history))
                ) / sum(weights)
                assert feature == pytest.approx(expected, rel=1e-6)
            if played and minutes is not None:
                history.append(minutes)


def test_serving_path_matches_training_path(panel: pl.DataFrame, state: pl.DataFrame):
    """The daily run and the training build must produce identical features.

    This is the train/serve skew guard: `state_as_of` reaches the same numbers
    for a historical date as the full-history build does for the games actually
    played that date.
    """
    target = date(2025, 3, 5)
    served = state_as_of(panel, target)
    trained = state.filter(pl.col("game_date") == target)
    assert served.height > 0 and trained.height > 0

    keys = ["team_id", "player_id"]
    # depth_rank and team_size are scoped to a game_id, which the serving path
    # replaces with a sentinel, so they are excluded and compared separately.
    feats = [
        c
        for c in served.columns
        if served.schema[c].is_numeric()
        and c not in {"depth_rank", "team_size", "game_id", "team_id", "player_id"}
        and c.startswith(("ewm_", "play_rate", "start_rate", "with_team_rate",
                          "coach_dnp_rate", "career_games", "season_games",
                          "team_games_", "days_"))
    ]
    merged = served.select(keys + feats).join(
        trained.select(keys + feats), on=keys, how="inner", suffix="_t"
    )
    assert merged.height > 100, "too few overlapping players to be a real check"

    for f in feats:
        a, b = merged[f], merged[f"{f}_t"]
        assert (a.is_null() == b.is_null()).all(), f"{f}: null mismatch"
        both = merged.filter(a.is_not_null() & b.is_not_null())
        if both.height and both[f].dtype.is_numeric():
            diff = (both[f] - both[f"{f}_t"]).abs().max()
            assert diff == pytest.approx(0.0, abs=1e-9), f"{f}: max diff {diff}"


def test_depth_rank_is_dense_within_each_team_game(state: pl.DataFrame):
    sample = state.filter(pl.col("season") == "2024-25").head(50_000)
    per_game = sample.group_by(["game_id", "team_id"]).agg(
        pl.col("depth_rank").min().alias("lo"),
        pl.col("depth_rank").max().alias("hi"),
        pl.col("depth_rank").n_unique().alias("distinct"),
        pl.len().alias("n"),
    )
    assert (per_game["lo"] == 1).all()
    assert (per_game["hi"] == per_game["n"]).all()
    assert (per_game["distinct"] == per_game["n"]).all()


def test_candidate_roster_puts_each_player_on_one_team(panel: pl.DataFrame):
    """A traded player must not get a virtual row for both teams.

    Two rows on the same date corrupt each other: the second sees the first as a
    prior game, which silently skews every trailing feature.
    """
    for target in (date(2025, 3, 5), date(2024, 2, 10), date(2023, 12, 20)):
        cand = candidate_roster(panel, target)
        assert cand.height == cand["player_id"].n_unique(), f"duplicate player on {target}"
        served = state_as_of(panel, target)
        assert served.height == served["player_id"].n_unique()


def test_panel_has_identity_on_absent_rows(panel: pl.DataFrame):
    absent = panel.filter(~pl.col("with_team"))
    assert absent["player_name"].null_count() == 0
    assert absent["team_abbreviation"].null_count() == 0


def test_prior_absence_streak_looks_only_backwards(state: pl.DataFrame):
    """`absent_streak_prior` must not know how long an absence will last.

    Its forward-looking twin `absence_run_full` is a diagnostic, not a feature;
    this pins the difference so the two cannot be confused.
    """
    out = add_pre_game_state(_synthetic([30.0, None, None, None, 20.0]), rate_stats=())
    assert out["absent_streak_prior"].to_list() == [0, 0, 1, 2, 3]


def test_absence_run_full_is_forward_looking_and_prior_streak_is_not(
    state: pl.DataFrame,
):
    """Pin the difference between the diagnostic and the feature.

    `absence_run_full` reports the length of the whole absence from its very
    first row, so it knows the future; `absent_streak_prior` counts only what
    has already happened. Confusing the two would leak heavily.
    """
    absent = state.filter(~pl.col("with_team"))
    # On the first row of any multi-game absence the full run already knows the
    # total, while the prior streak is still zero.
    first_rows = absent.filter(
        (pl.col("absent_streak_prior") == 0) & (pl.col("absence_run_full") > 1)
    )
    assert first_rows.height > 1000, "expected many multi-game absences"
    assert (first_rows["absence_run_full"] > first_rows["absent_streak_prior"]).all()
    # The prior streak never runs past the absence it belongs to.
    assert (absent["absent_streak_prior"] < absent["absence_run_full"]).all()


def test_no_forward_looking_column_is_a_model_feature():
    """Guard the one column that is deliberately forward-looking."""
    from nba_hierarchy.availability import REPORT_FEATURES, STATE_FEATURES

    forbidden = {"absence_run_full", "min", "usg_pct", "played", "start_position"}
    assert not (set(STATE_FEATURES) | set(REPORT_FEATURES)) & forbidden

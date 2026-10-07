"""Compose the layers into one frame, without letting a later layer see ahead.

Each layer consumes the one before it, so a naive composition leaks twice over:
a position or availability model fitted on all seasons has seen the test season,
and absorption features built from an availability model's *in-sample*
predictions are sharper than anything the daily run will ever get.

Both are handled here. Layer 2 and 3 models are fitted only on seasons that
finished before the earliest test season, and `p_play` is produced out of fold
-- for each season, from a model fitted on seasons before it -- so the
probability feeding layer 4's features is as noisy in training as in service.
"""

from __future__ import annotations

from datetime import date
from pathlib import Path

import polars as pl

from .absorption import (
    ABSORPTION_FEATURES,
    MINUTES_TARGET,
    OWN_FEATURES,
    USAGE_TARGET,
    add_absorption_features,
    fit_regressor,
    predict,
)
from .availability import (
    REPORT_FEATURES,
    STATE_FEATURES,
    add_report_features,
    fit_calibrated_play_model,
    injury_era,
)
from .config import DATA_DIR, SEASON_TYPES
from .data import load_fixtures, load_player_games
from .position import (
    HISTORY_FEATURES,
    PROFILE_FEATURES,
    SLOTS,
    assign_positions,
    fit_slot_model,
    predict_slots,
    prepare,
)
from .roster import build_panel, load_injury_reports
from .state import add_pre_game_state, append_serving_rows, serving_rows

PLAY_FEATURES = STATE_FEATURES + REPORT_FEATURES


def base_frame(
    data_dir: Path = DATA_DIR, season_types: tuple[str, ...] = SEASON_TYPES
) -> pl.DataFrame:
    """Layers 1-2 inputs: the panel, its pre-game state, and report features."""
    panel = build_panel(data_dir, season_types)
    return add_report_features(prepare(add_pre_game_state(panel), data_dir))


def add_positions(data: pl.DataFrame, fit_before: str) -> pl.DataFrame:
    """Fit the slot model on seasons before `fit_before`, then assign positions."""
    train = data.filter(
        pl.col("start_position").is_in(SLOTS) & (pl.col("season") < fit_before)
    )
    features = PROFILE_FEATURES + HISTORY_FEATURES
    model = fit_slot_model(train, features)
    return assign_positions(predict_slots(model, data, features))


def add_out_of_fold_play_probability(data: pl.DataFrame) -> pl.DataFrame:
    """Attach `p_play` fitted only on earlier seasons, season by season.

    The first injury-era season has nothing before it to train on, so it yields
    no probability and drops out. That is 2021-22, whose report is in any case
    not comparable to later seasons.
    """
    seasons = sorted(data["season"].unique().to_list())
    scored: list[pl.DataFrame] = []
    for season in seasons[1:]:
        train = data.filter(pl.col("season") < season)
        test = data.filter(pl.col("season") == season)
        if not train.height or not test.height:
            continue
        model = fit_calibrated_play_model(train, PLAY_FEATURES)
        scored.append(test.with_columns(p_play=pl.Series(model.predict(test))))
    if not scored:
        raise ValueError("no season had an earlier season to train on")
    return pl.concat(scored, how="vertical")


def layer4_frame(
    data_dir: Path = DATA_DIR,
    fit_positions_before: str = "2024-25",
    season_types: tuple[str, ...] = SEASON_TYPES,
) -> pl.DataFrame:
    """Everything layer 4 needs: positions, out-of-fold `p_play`, absorption.

    Restricted to the injury era, since the absence features are built from a
    report-informed availability model. Extending further back would mean a
    state-only availability model, which is the obvious lever if sample size
    turns out to bind.
    """
    data = add_positions(base_frame(data_dir, season_types), fit_positions_before)
    return add_absorption_features(add_out_of_fold_play_probability(injury_era(data)))


def _attach_report_for_date(
    combined: pl.DataFrame, as_of_date: date, data_dir: Path
) -> pl.DataFrame:
    """Fill the target date's rows with that date's injury report.

    The virtual rows arrive with a null `report_status` because they were not in
    the panel when `build_panel` joined the report. Without this the daily run
    would treat every player as unreported -- the single most consequential
    difference between what it sees and what it was trained on.
    """
    report = load_injury_reports(data_dir).filter(pl.col("game_date") == as_of_date)
    return (
        combined.join(
            report.select(
                "team_id", "player_id", pl.col("status").alias("_report_today")
            ),
            on=["team_id", "player_id"],
            how="left",
        )
        .with_columns(
            report_status=pl.when(pl.col("game_date") == as_of_date)
            .then(pl.col("_report_today"))
            .otherwise(pl.col("report_status"))
        )
        .drop("_report_today")
    )


def daily_hierarchy(
    as_of_date: date,
    data_dir: Path = DATA_DIR,
    season_types: tuple[str, ...] = SEASON_TYPES,
    lookback_games: int = 10,
    require_fixture: bool = True,
) -> pl.DataFrame:
    """The daily run: a ranked, position-assigned depth chart for every team.

    Each layer's model is fitted only on games before `as_of_date`, and the
    target date's rows ride through the same transformations as the training
    history, so nothing in the output depends on code that only runs in service.

    Output rows carry the fixture they describe -- `game_date`, `game_id`,
    `opponent`, `home` -- because a depth chart with no game attached to it is
    not interpretable once separated from its filename.

    `require_fixture` keeps only teams that actually play on `as_of_date`, which
    is the default because everything else about these rows refers to that date:
    the injury report is read for it, and the features are computed up to it. On
    a typical night a third of the league plays, and without this the run emits
    a chart for all thirty teams -- inventing a game for twenty of them. Pass
    False only to inspect a team's standing on a date it is idle, and read the
    result as hypothetical.
    """
    panel = build_panel(data_dir, season_types)
    combined = _attach_report_for_date(
        append_serving_rows(panel, as_of_date, lookback_games), as_of_date, data_dir
    )
    data = add_report_features(prepare(add_pre_game_state(combined), data_dir))

    before = pl.col("game_date") < as_of_date
    position_features = PROFILE_FEATURES + HISTORY_FEATURES
    slot_model = fit_slot_model(
        data.filter(before & pl.col("start_position").is_in(SLOTS)), position_features
    )
    data = assign_positions(predict_slots(slot_model, data, position_features))

    # Re-cut the history only once positions exist, since layer 4's features are
    # position-aware and the absorption frame is built from this slice.
    history = data.filter(before)
    play_model = fit_calibrated_play_model(injury_era(history), PLAY_FEATURES)
    data = add_absorption_features(
        data.with_columns(p_play=pl.Series(play_model.predict(data)))
    )

    # Layer 4 trains on the same out-of-fold frame the evaluation validated.
    train = add_absorption_features(
        add_out_of_fold_play_probability(injury_era(history))
    ).filter(pl.col("played"))
    features = OWN_FEATURES + ABSORPTION_FEATURES
    minutes_model = fit_regressor(train, features, MINUTES_TARGET)
    usage_model = fit_regressor(train, features, USAGE_TARGET)

    served = serving_rows(data)
    fixtures = load_fixtures(data_dir, season_types).filter(
        pl.col("game_date") == as_of_date
    )
    served = served.drop("game_id").join(
        fixtures.select("team_id", "game_id", "opponent", "home", "matchup"),
        on="team_id",
        how="inner" if require_fixture else "left",
    )
    served = predict(minutes_model, served, features, "minutes_if_plays")
    served = predict(usage_model, served, features, "usage_if_plays")
    return served.with_columns(
        expected_minutes=pl.col("p_play") * pl.col("minutes_if_plays").clip(0.0),
        expected_usage=pl.col("usage_if_plays").clip(0.0, 1.0),
    ).sort(["team_abbreviation", "position", "position_depth"])

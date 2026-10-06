"""Print a team's depth chart for a date: the layer 1 + 2 output end to end."""

from __future__ import annotations

import sys
from datetime import date

import polars as pl

from nba_hierarchy.data import load_player_games
from nba_hierarchy.position import (
    HISTORY_FEATURES,
    POSITIONS,
    PROFILE_FEATURES,
    SLOTS,
    assign_positions,
    fit_slot_model,
    predict_slots,
    prepare,
)
from nba_hierarchy.roster import build_panel
from nba_hierarchy.state import add_pre_game_state, state_as_of


def main(team: str = "BOS", as_of: date = date(2025, 3, 5)) -> None:
    panel = build_panel(pg=load_player_games())
    state = prepare(add_pre_game_state(panel))

    features = PROFILE_FEATURES + HISTORY_FEATURES
    train = state.filter(
        pl.col("start_position").is_in(SLOTS) & (pl.col("game_date") < as_of)
    )
    model = fit_slot_model(train, features)

    served = prepare(state_as_of(panel, as_of))
    scored = assign_positions(predict_slots(model, served, features))

    pl.Config.set_tbl_rows(30)
    pl.Config.set_tbl_width_chars(130)
    chart = (
        scored.filter(pl.col("team_abbreviation") == team)
        .sort(
            [pl.col("position").replace_strict({p: i for i, p in enumerate(POSITIONS)}),
             "position_depth"]
        )
        .select(
            "position",
            pl.col("position_depth").alias("depth"),
            pl.col("player_name").str.slice(0, 20).alias("player"),
            pl.col("height_cm").cast(pl.Int32).alias("cm"),
            pl.col("ewm_min_8").round(1).alias("min"),
            pl.col("ewm_min_played_8").round(1).alias("min_fit"),
            pl.col("ewm_usg_pct_8").round(3).alias("usg"),
            pl.col("ewm_ast_pct_20").round(3).alias("ast_pct"),
            pl.col("size_score").round(2).alias("size"),
            pl.col("play_rate_10").round(2).alias("play_r"),
        )
    )
    print(f"\n=== {team} depth chart as of {as_of} ===")
    print(chart)


if __name__ == "__main__":
    main(*(sys.argv[1:] or ["BOS"]))

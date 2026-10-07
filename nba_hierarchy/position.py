"""Layer 2: assign each player to one of the five basketball positions.

No source in this database carries PG/SG/SF/PF/C. Every position field --
`player_info.position`, both roster tables, `usage_shock.position` -- is G/F/C
with hyphenated combinations. The five positions have to be derived.

What the data does provide is the NBA's own slot structure: `start_position`
resolves to exactly C/F/F/G/G in 45,270 of 45,276 team games, and a player's
starting slot matches his modal slot 95.0% of the time. That is a real, stable,
labelled 2G/2F/1C skeleton going back to 2009-10.

So the two halves of this module are different in kind, and it matters:

* The coarse slot (G/F/C) is *learned*, supervised on `start_position`, and
  validated against baselines on held-out seasons.
* The PG/SG and SF/PF splits are *imposed*. No labels for them exist anywhere,
  so they are a documented convention -- among a team's guards the better
  playmaker is the point guard, among its forwards the bigger is the power
  forward -- applied relative to teammates, which is how a depth chart works.
  Treat the split as a reading of the roster, not as a fact recovered from data.

Usage is excluded from every input here: it is the target of the wider project,
and a position model that keys on it would leak role into the thing being
predicted.
"""

from __future__ import annotations

from pathlib import Path

import numpy as np
import polars as pl

from .config import DATA_DIR
from .state import canonical_sort

SLOTS = ("G", "F", "C")
POSITIONS = ("PG", "SG", "SF", "PF", "C")

# Starters per team game, by slot. The skeleton every assignment respects.
SLOT_QUOTA = {"G": 2, "F": 2, "C": 1}

# Half-life for the positional profile. Longer than the depth-chart anchor:
# what a player *is* moves far more slowly than where he sits in the rotation.
PROFILE_HL = 20

# Positional inputs. No usage, no raw minutes -- those describe standing in the
# rotation, not role.
PROFILE_FEATURES = (
    "height_cm",
    f"ewm_ast_pct_{PROFILE_HL}",
    f"ewm_reb_pct_{PROFILE_HL}",
    f"ewm_oreb_pct_{PROFILE_HL}",
    f"ewm_dreb_pct_{PROFILE_HL}",
    f"ewm_ast36_{PROFILE_HL}",
    f"ewm_reb36_{PROFILE_HL}",
    f"ewm_blk36_{PROFILE_HL}",
    f"ewm_stl36_{PROFILE_HL}",
    f"ewm_fga36_{PROFILE_HL}",
    f"ewm_fg3_rate_{PROFILE_HL}",
    f"ewm_ftr_{PROFILE_HL}",
)

# Where a player has started before, as a share of his prior starts. Strong, but
# undefined for anyone yet to start, which is why it supplements the profile
# rather than replacing it.
HISTORY_FEATURES = tuple(f"prior_start_share_{s}" for s in SLOTS)


def load_player_attributes(data_dir: Path = DATA_DIR) -> pl.DataFrame:
    """Static per-player attributes. Height is constant, so season is ignored.

    `player_info` is a directory of 5,161 players snapshotted in two seasons,
    not a per-season table; joining on player_id covers all history at 99.6% of
    played minutes. A handful of players are listed at two heights, so the
    median is taken.
    """
    info = pl.read_parquet(data_dir / "nba" / "player_info.parquet")
    return (
        info.select("player_id", "height_cm", "weight_kg")
        .drop_nulls("height_cm")
        .group_by("player_id")
        .agg(pl.col("height_cm").median(), pl.col("weight_kg").median())
    )


def add_start_history(panel: pl.DataFrame) -> pl.DataFrame:
    """Each player's prior starting slots, counted strictly before each row."""
    counts = [
        (pl.col("start_position") == s)
        .cast(pl.Int32)
        .cum_sum()
        .shift(1)
        .fill_null(0)
        .over("player_id")
        .alias(f"prior_starts_{s}")
        for s in SLOTS
    ]
    out = canonical_sort(panel).with_columns(counts)
    total = pl.sum_horizontal(f"prior_starts_{s}" for s in SLOTS)
    return out.with_columns(
        [total.alias("prior_starts_total")]
        + [
            pl.when(total > 0)
            .then(pl.col(f"prior_starts_{s}") / total)
            .otherwise(None)
            .alias(f"prior_start_share_{s}")
            for s in SLOTS
        ]
    )


def prepare(state: pl.DataFrame, data_dir: Path = DATA_DIR) -> pl.DataFrame:
    """Attach height and prior-start history to the layer 1 state."""
    return add_start_history(state).join(
        load_player_attributes(data_dir), on="player_id", how="left"
    )


def _matrix(df: pl.DataFrame, features: tuple[str, ...]) -> np.ndarray:
    return df.select(features).to_numpy().astype(np.float64)


def fit_slot_model(train: pl.DataFrame, features: tuple[str, ...], **kwargs):
    """Train the G/F/C classifier on rows where a player actually started."""
    import lightgbm as lgb

    labelled = train.filter(pl.col("start_position").is_in(SLOTS))
    y = np.array([SLOTS.index(s) for s in labelled["start_position"]])
    params = {
        "objective": "multiclass",
        "num_class": len(SLOTS),
        "learning_rate": 0.05,
        "num_leaves": 31,
        "min_child_samples": 50,
        "n_estimators": 300,
        "verbose": -1,
        "random_state": 0,
    }
    params.update(kwargs)
    model = lgb.LGBMClassifier(**params)
    model.fit(_matrix(labelled, features), y)
    return model


def predict_slots(
    model, state: pl.DataFrame, features: tuple[str, ...]
) -> pl.DataFrame:
    """Attach slot probabilities, the argmax slot, and a continuous size score."""
    proba = model.predict_proba(_matrix(state, features))
    out = state.with_columns(
        [pl.Series(f"p_{s}", proba[:, i]) for i, s in enumerate(SLOTS)]
    )
    return out.with_columns(
        slot=pl.Series([SLOTS[i] for i in proba.argmax(axis=1)]),
        # -1 is an unambiguous guard, +1 an unambiguous centre. The spectrum
        # that orders the frontcourt, learned rather than stipulated.
        size_score=pl.Series(proba[:, SLOTS.index("C")] - proba[:, SLOTS.index("G")]),
    )


def assign_positions(state: pl.DataFrame) -> pl.DataFrame:
    """Split slots into the five positions, producing a depth chart per team.

    Within each team's guards the better playmakers are the point guards; within
    its forwards the bigger are the power forwards. Centres need no split. The
    convention is relative to teammates -- a team's most pass-oriented guards are
    its point guards whatever their absolute assist rate -- and `position_depth`
    then ranks players inside a position by trailing minutes.

    Nothing in the data labels these five positions, so this is a documented
    reading of the roster, not a fact recovered from the source. Its soundness
    is bounded by how far apart the rule's two sides actually are at the point
    it cuts, and often that is barely at all. Measured on 2024-25 and 2025-26,
    the gap between the last player on the lead side and the first on the other
    has a median of 0.040 in assist percentage for guards and 0.064 in size
    score for forwards -- and 28.4% of guard boundaries and 22.7% of forward
    boundaries fall below 0.02, which is close enough to call arbitrary.

    Game to game, a player keeps the same position 87.5% of the time against
    97.1% for the coarse G/F/C slot. Centres, who need no split, hold 97.2%;
    the split positions run from 89.6% for point guards down to 80.7% for small
    forwards, and nearly all the churn is PG<->SG and SF<->PF. Treat the coarse
    slot as reliable and the split as indicative.

    An earlier version paired players into tiers of two by minutes and split
    inside each pair, which was worse (85.6%): when two guards swapped minutes
    ranks the pairings changed and both flipped position for reasons having
    nothing to do with either player. Splitting against the whole slot avoids
    that coupling.
    """
    group = ["game_id", "team_id"]
    by_slot = group + ["slot"]
    playmaking = pl.col(f"ewm_ast_pct_{PROFILE_HL}").fill_null(0.0)
    bigness = pl.col("size_score").fill_null(0.0)

    # Playmaking orders guards, size orders forwards.
    key = pl.when(pl.col("slot") == "G").then(playmaking).otherwise(bigness)
    # `rank("ordinal")` breaks ties by row order, so the frame needs a
    # reproducible one before any ranking.
    out = canonical_sort(state).with_columns(
        _key_rank=key.rank("ordinal", descending=True).over(by_slot),
        _slot_n=pl.len().over(by_slot),
    )
    takes_lead = pl.col("_key_rank") <= (pl.col("_slot_n") + 1) // 2

    out = out.with_columns(
        position=pl.when(pl.col("slot") == "C")
        .then(pl.lit("C"))
        .when(pl.col("slot") == "G")
        .then(pl.when(takes_lead).then(pl.lit("PG")).otherwise(pl.lit("SG")))
        .otherwise(pl.when(takes_lead).then(pl.lit("PF")).otherwise(pl.lit("SF")))
    ).drop("_key_rank", "_slot_n")

    return out.with_columns(
        position_depth=pl.col("ewm_min_8")
        .fill_null(0.0)
        .rank("ordinal", descending=True)
        .over(group + ["position"])
        .cast(pl.Int32)
    )


def constrained_slot_assignment(
    df: pl.DataFrame, group: tuple[str, ...] = ("game_id", "team_id")
) -> pl.DataFrame:
    """Assign a lineup to the 2G/2F/1C skeleton by minimum-cost matching.

    Taking each player's most likely slot independently throws away the one
    hard fact the data guarantees: a starting five is two guards, two forwards
    and a centre, in 45,270 of 45,276 team games. Matching the five players to
    the five slots jointly lets a confident centre push an ambiguous big to
    forward, which per-player argmax cannot do.

    Groups that are not five players are left at their argmax slot.
    """
    from scipy.optimize import linear_sum_assignment

    slot_order = [s for s, n in SLOT_QUOTA.items() for _ in range(n)]
    df = df.sort(list(group))
    proba = np.column_stack([df[f"p_{s}"].to_numpy() for s in SLOTS])
    # Cost of putting a player in a slot. Clipped so a zero probability is
    # expensive rather than infinite, which would break the solver.
    cost_all = -np.log(np.clip(proba, 1e-12, None))
    slot_idx = np.array([SLOTS.index(s) for s in slot_order])

    assigned = df["slot"].to_numpy().copy()
    sizes = df.group_by(list(group), maintain_order=True).len()["len"].to_numpy()
    start = 0
    for n in sizes:
        if n == len(slot_order):
            block = cost_all[start : start + n][:, slot_idx]
            rows, cols = linear_sum_assignment(block)
            for r, c in zip(rows, cols):
                assigned[start + r] = slot_order[c]
        start += n

    return df.with_columns(slot=pl.Series(assigned))

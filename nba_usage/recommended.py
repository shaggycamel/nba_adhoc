"""What survived its ablation, in one place.

Nine feature blocks were built and tested against time-based folds. Two are
in the recommended configuration; the rest are kept in the repository so
their negative results stay reproducible, but they are not used here.

    kept      injury absence, rotation position, standings (minutes only)
    rejected  role clusters, pairwise absorption, component decomposition,
              absence duration, player biography, opponent form, game
              mismatch, roster churn

Three models are recommended, each for a different target:

    usage    Ridge on lagged history + absence + rotation
    minutes  LightGBM on the same, plus standings
    volume   predicted minutes x a predicted per-minute rate

Minutes is the one worth having. It lifts R2 from 0.25 to 0.47 on the case
the project cares about -- a backup filling in for an injured starter --
where the usage model's edge over a simple average is far smaller.
"""

from __future__ import annotations

import polars as pl

from .features import feature_columns
from .hierarchy import ROTATION_COLS
from .injuries import ABSENCE_COLS
from .models import fit_lightgbm, fit_ridge
from .standings import STANDINGS_COLS
from .volume import volume_feature_columns

# Blocks that failed and must not be reintroduced by a wildcard match.
REJECTED_PREFIXES = (
    "role_", "vacated_same_role", "vacated_other_role",   # role clusters
    "absorb_", "n_absent_known",                          # pairwise absorption
    "opp_", "game_pace",                                  # opponent form
    "net_rating_diff", "win_pct_diff", "mismatch_size", "expected_edge",
    "days_since_acquired", "recently_acquired", "team_arrivals", "team_departures",
    "height_cm", "weight_kg", "season_exp", "draft_pick", "age_years",
    "is_guard", "is_forward", "is_center",
    "absent_exp_remaining", "absent_games_elapsed", "absent_severe",
    "absent_fresh", "games_since_return",
)

# Own-team rolling form, excluded by exact name: a bare "own_" prefix would
# also catch own_status_rank, which is a kept absence feature.
REJECTED_EXACT = {
    f"own_{s}" for s in
    ("pace", "poss", "off_rating", "def_rating", "dreb_pct", "oreb_pct", "pts", "reb", "ast")
}

# Targets, never inputs.
TARGETS = {"usg_pct", "min", "pts", "reb", "ast", "fga_pm", "fta_pm", "tov_pm",
           "pts_pm", "reb_pm", "ast_pm", "player_rate", "team_rate", "usg_recon"}


def _clean(cols: list[str]) -> list[str]:
    out = [
        c for c in dict.fromkeys(cols)
        if c not in TARGETS
        and c not in REJECTED_EXACT
        and not c.startswith(REJECTED_PREFIXES)
    ]
    return out


def usage_columns(frame: pl.DataFrame) -> list[str]:
    """Lagged history, injury absence and rotation position."""
    extra = [c for c in ABSENCE_COLS + ROTATION_COLS if c in frame.columns]
    return _clean([c for c in feature_columns(frame) + extra if c in frame.columns])


def minutes_columns(frame: pl.DataFrame) -> list[str]:
    """As above, plus the minutes-allocation terms and standings."""
    from .minutes import MINUTES_COLS

    extra = [
        c for c in ABSENCE_COLS + ROTATION_COLS + MINUTES_COLS + STANDINGS_COLS
        if c in frame.columns
    ]
    return _clean([c for c in volume_feature_columns(frame) + extra if c in frame.columns])


def volume_columns(frame: pl.DataFrame) -> list[str]:
    """Same inputs as minutes; the rate model also takes a usage prediction."""
    return minutes_columns(frame)


# The model each target should be fitted with, chosen on the folds.
USAGE_MODEL = fit_ridge
MINUTES_MODEL = fit_lightgbm
VOLUME_MODEL = fit_lightgbm

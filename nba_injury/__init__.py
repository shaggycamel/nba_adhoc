"""Injury-duration modelling for the NBA injury report.

The package turns the per-game injury report (`nba.injuries`) into
player-level injury *spells*, attaches features that are knowable at the
moment a player is first ruled out, and fits models that predict how long
the spell will last.
"""

__all__ = [
    "paths",
    "taxonomy",
    "calendar",
    "spells",
    "features",
    "hazard",
    "models",
]

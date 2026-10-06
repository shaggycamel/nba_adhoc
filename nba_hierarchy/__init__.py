"""Team-hierarchy model: ranks each team's players into the five position slots.

Layers, built and validated independently:

1. `data` / `roster` / `state` — the point-in-time player panel and the
   pre-game feature state, computed strictly from prior games.
2. position assignment onto the 2G/2F/1C starter skeleton.
3. availability from the injury report.
4. minutes and usage absorption when a teammate is absent.
"""

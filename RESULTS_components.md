# Component decomposition

Usage rearranges to the player's event rate per minute divided by the team's
event rate per game-minute, so it can be composed from predicted parts instead
of predicted directly. Same features, same folds, three parameterisations.

Reconstructing `usg_pct` from the **actual observed** components misses the stored
column by 0.01379 MAE (median 0.00728, correlation 0.9463).
Minutes are whole numbers in this dataset and usage is stored to three decimals,
so that is the floor any composed approach inherits before a model is fitted.

| fold | model | approach | mae | r2 |
|---|---|---|---|---|
| 2024-25 | baseline (ewm3) | - | 0.05173 | 0.29362 |
| 2024-25 | ridge | direct | 0.04945 | 0.35648 |
| 2024-25 | ridge | rate ratio | 0.05103 | 0.32994 |
| 2024-25 | ridge | components | 0.05103 | 0.32994 |
| 2024-25 | lightgbm | direct | 0.04964 | 0.35385 |
| 2024-25 | lightgbm | rate ratio | 0.05112 | 0.32460 |
| 2024-25 | lightgbm | components | 0.05131 | 0.32300 |
| 2025-26 | baseline (ewm3) | - | 0.05099 | 0.31769 |
| 2025-26 | ridge | direct | 0.04888 | 0.37219 |
| 2025-26 | ridge | rate ratio | 0.05038 | 0.34613 |
| 2025-26 | ridge | components | 0.05038 | 0.34613 |
| 2025-26 | lightgbm | direct | 0.04902 | 0.36942 |
| 2025-26 | lightgbm | rate ratio | 0.05044 | 0.34022 |
| 2025-26 | lightgbm | components | 0.05060 | 0.33990 |

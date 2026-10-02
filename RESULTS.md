# Results

Target `usg_pct`, 97 features, injury-era seasons only (2021-22 onward, the first season the injury report covers).
Expanding-window season folds; no shuffling anywhere.

| fold | kind | model | MAE | RMSE | R2 | n |
| --- | --- | --- | --- | --- | --- | --- |
| 2024-25 | model | ridge | 0.04945 | 0.06878 | 0.3565 | 25025 |
| 2024-25 | model | elasticnet | 0.04948 | 0.06882 | 0.3557 | 25025 |
| 2024-25 | model | random_forest | 0.04957 | 0.06878 | 0.3565 | 25025 |
| 2024-25 | model | catboost | 0.04961 | 0.06884 | 0.3552 | 25025 |
| 2024-25 | model | xgboost | 0.04962 | 0.06893 | 0.3537 | 25025 |
| 2024-25 | model | extra_trees | 0.04965 | 0.06882 | 0.3558 | 25025 |
| 2024-25 | model | lightgbm | 0.04965 | 0.06887 | 0.3548 | 25025 |
| 2024-25 | model | mlp_embeddings | 0.05035 | 0.06957 | 0.3416 | 25025 |
| 2024-25 | model | gru_sequence | 0.05086 | 0.07014 | 0.3308 | 25025 |
| 2024-25 | baseline | ewm3 | 0.05173 | 0.07206 | 0.2936 | 25025 |
| 2024-25 | baseline | last10_mean | 0.05184 | 0.07202 | 0.2944 | 25025 |
| 2024-25 | baseline | season_mean | 0.05214 | 0.07300 | 0.2750 | 25025 |
| 2024-25 | baseline | last5_mean | 0.05359 | 0.07443 | 0.2464 | 25025 |
| 2024-25 | baseline | career_mean | 0.05462 | 0.07453 | 0.2444 | 25025 |
| 2024-25 | baseline | last3_mean | 0.05605 | 0.07776 | 0.1774 | 25025 |
| 2024-25 | baseline | global_mean | 0.06629 | 0.08574 | -0.0001 | 25025 |
| 2025-26 | model | elasticnet | 0.04886 | 0.06672 | 0.3718 | 26065 |
| 2025-26 | model | ridge | 0.04888 | 0.06670 | 0.3722 | 26065 |
| 2025-26 | model | catboost | 0.04892 | 0.06672 | 0.3718 | 26065 |
| 2025-26 | model | xgboost | 0.04892 | 0.06674 | 0.3715 | 26065 |
| 2025-26 | model | random_forest | 0.04894 | 0.06678 | 0.3707 | 26065 |
| 2025-26 | model | lightgbm | 0.04900 | 0.06683 | 0.3698 | 26065 |
| 2025-26 | model | extra_trees | 0.04904 | 0.06682 | 0.3700 | 26065 |
| 2025-26 | model | mlp_embeddings | 0.04933 | 0.06741 | 0.3589 | 26065 |
| 2025-26 | model | gru_sequence | 0.04957 | 0.06753 | 0.3566 | 26065 |
| 2025-26 | baseline | ewm3 | 0.05099 | 0.06954 | 0.3177 | 26065 |
| 2025-26 | baseline | last10_mean | 0.05111 | 0.06951 | 0.3182 | 26065 |
| 2025-26 | baseline | season_mean | 0.05141 | 0.07020 | 0.3046 | 26065 |
| 2025-26 | baseline | last5_mean | 0.05298 | 0.07193 | 0.2699 | 26065 |
| 2025-26 | baseline | career_mean | 0.05398 | 0.07218 | 0.2648 | 26065 |
| 2025-26 | baseline | last3_mean | 0.05537 | 0.07524 | 0.2013 | 26065 |
| 2025-26 | baseline | global_mean | 0.06561 | 0.08418 | -0.0000 | 26065 |

# Processing experiments

All variants scored with Ridge on the same expanding-window season folds.

## Window length and weighting (Ridge, mean over folds)

| variant | n_features | mae | r2 |
|---|---|---|---|
| long (5,10,20,40) | 105 | 0.04914 | 0.36482 |
| ewma only | 89 | 0.04915 | 0.36443 |
| default (3,5,10,20) | 97 | 0.04917 | 0.36434 |
| medium (3,5,10) | 89 | 0.04917 | 0.36410 |
| short (3,5) | 73 | 0.04931 | 0.36135 |

## Injury encodings (Ridge, mean over folds)

| encoding | n_features | mae | r2 |
|---|---|---|---|
| all absence + rotation | 97 | 0.04917 | 0.36434 |
| all absence | 89 | 0.04950 | 0.35847 |
| share only | 80 | 0.04969 | 0.35600 |
| load only | 80 | 0.04969 | 0.35590 |
| count only | 80 | 0.04977 | 0.35364 |
| own status only | 80 | 0.04978 | 0.35299 |
| none | 79 | 0.04978 | 0.35298 |

## Outlier and DNP handling (row sets differ, so MAE is not comparable across rows)

| treatment | n_rows | mae | r2 |
|---|---|---|---|
| all played rows | 128652 | 0.04917 | 0.36434 |
| min >= 5 | 118977 | 0.04339 | 0.46531 |
| min >= 10 | 109745 | 0.04111 | 0.50626 |
| usage winsorised 1-99% | 128652 | 0.04844 | 0.38467 |
| >= 5 prior games | 114899 | 0.04760 | 0.39048 |

## Greedy forward selection (Ridge, validated on the last season)

| n | added | mae | r2 |
|---|---|---|---|
| 1 | usg_pct_ewm10 | 0.05007 | 0.34618 |
| 2 | expected_load_share | 0.04949 | 0.35712 |
| 3 | min_r10 | 0.04916 | 0.36474 |
| 4 | usg_pct_season | 0.04911 | 0.36570 |
| 5 | fga_sd20 | 0.04907 | 0.36615 |
| 6 | usg_pct_sd20 | 0.04904 | 0.36645 |
| 7 | tov_pct_r10 | 0.04901 | 0.36693 |
| 8 | ast_pct_season | 0.04899 | 0.36748 |
| 9 | ts_pct_career | 0.04898 | 0.36766 |
| 10 | fta_r3 | 0.04896 | 0.36798 |
| 11 | usg_pct_r10 | 0.04895 | 0.36829 |
| 12 | usg_pct_ewm3 | 0.04893 | 0.36845 |

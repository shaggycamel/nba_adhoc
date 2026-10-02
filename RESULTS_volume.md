# Volume projection

Points, rebounds and assists. Same folds and discipline as the usage work;
the usage prediction fed to the rate models is out-of-fold.

| fold | stat | slice | method | mae | rmse | r2 | n |
|---|---|---|---|---|---|---|---|
| 2024-25 | ast | all rows | direct | 1.3152 | 1.7870 | 0.5339 | 25025 |
| 2024-25 | ast | all rows | pipeline (min x rate, usage fed) | 1.3200 | 1.7922 | 0.5312 | 25025 |
| 2024-25 | ast | all rows | baseline season_mean | 1.3631 | 1.8712 | 0.4890 | 25025 |
| 2024-25 | ast | all rows | baseline last10_mean | 1.3731 | 1.8690 | 0.4902 | 25025 |
| 2024-25 | ast | all rows | baseline last5_mean | 1.4064 | 1.9247 | 0.4593 | 25025 |
| 2025-26 | ast | all rows | direct | 1.3308 | 1.7992 | 0.5074 | 26065 |
| 2025-26 | ast | all rows | pipeline (min x rate, usage fed) | 1.3328 | 1.8021 | 0.5058 | 26065 |
| 2025-26 | ast | all rows | baseline season_mean | 1.3792 | 1.8898 | 0.4566 | 26065 |
| 2025-26 | ast | all rows | baseline last10_mean | 1.3795 | 1.8809 | 0.4617 | 26065 |
| 2025-26 | ast | all rows | baseline last5_mean | 1.4089 | 1.9252 | 0.4361 | 26065 |
| 2024-25 | ast | starter out | direct | 1.3213 | 1.7839 | 0.5190 | 16058 |
| 2024-25 | ast | starter out | pipeline (min x rate, usage fed) | 1.3253 | 1.7901 | 0.5156 | 16058 |
| 2024-25 | ast | starter out | baseline season_mean | 1.3612 | 1.8733 | 0.4695 | 16058 |
| 2024-25 | ast | starter out | baseline last10_mean | 1.3693 | 1.8656 | 0.4739 | 16058 |
| 2024-25 | ast | starter out | baseline last5_mean | 1.4014 | 1.9164 | 0.4449 | 16058 |
| 2025-26 | ast | starter out | direct | 1.3445 | 1.8151 | 0.4998 | 20472 |
| 2025-26 | ast | starter out | pipeline (min x rate, usage fed) | 1.3447 | 1.8171 | 0.4987 | 20472 |
| 2025-26 | ast | starter out | baseline last10_mean | 1.3849 | 1.8947 | 0.4550 | 20472 |
| 2025-26 | ast | starter out | baseline season_mean | 1.3865 | 1.9061 | 0.4483 | 20472 |
| 2025-26 | ast | starter out | baseline last5_mean | 1.4136 | 1.9352 | 0.4314 | 20472 |
| 2024-25 | ast | starter out, player is a backup | pipeline (min x rate, usage fed) | 1.0862 | 1.4710 | 0.3164 | 8731 |
| 2024-25 | ast | starter out, player is a backup | direct | 1.0863 | 1.4759 | 0.3118 | 8731 |
| 2024-25 | ast | starter out, player is a backup | baseline last10_mean | 1.1150 | 1.5585 | 0.2326 | 8731 |
| 2024-25 | ast | starter out, player is a backup | baseline season_mean | 1.1200 | 1.5672 | 0.2240 | 8731 |
| 2024-25 | ast | starter out, player is a backup | baseline last5_mean | 1.1358 | 1.5911 | 0.2002 | 8731 |
| 2025-26 | ast | starter out, player is a backup | direct | 1.1214 | 1.5214 | 0.3387 | 11346 |
| 2025-26 | ast | starter out, player is a backup | pipeline (min x rate, usage fed) | 1.1218 | 1.5192 | 0.3406 | 11346 |
| 2025-26 | ast | starter out, player is a backup | baseline last10_mean | 1.1504 | 1.6078 | 0.2615 | 11346 |
| 2025-26 | ast | starter out, player is a backup | baseline season_mean | 1.1593 | 1.6207 | 0.2496 | 11346 |
| 2025-26 | ast | starter out, player is a backup | baseline last5_mean | 1.1685 | 1.6335 | 0.2377 | 11346 |
| 2024-25 | pts | all rows | direct | 4.4530 | 5.8138 | 0.5622 | 25025 |
| 2024-25 | pts | all rows | pipeline (min x rate, usage fed) | 4.4599 | 5.8412 | 0.5581 | 25025 |
| 2024-25 | pts | all rows | baseline last10_mean | 4.7091 | 6.1508 | 0.5100 | 25025 |
| 2024-25 | pts | all rows | baseline season_mean | 4.7242 | 6.2047 | 0.5014 | 25025 |
| 2024-25 | pts | all rows | baseline last5_mean | 4.8357 | 6.3469 | 0.4783 | 25025 |
| 2025-26 | pts | all rows | direct | 4.4754 | 5.8349 | 0.5435 | 26065 |
| 2025-26 | pts | all rows | pipeline (min x rate, usage fed) | 4.4764 | 5.8535 | 0.5405 | 26065 |
| 2025-26 | pts | all rows | baseline last10_mean | 4.6986 | 6.1635 | 0.4906 | 26065 |
| 2025-26 | pts | all rows | baseline season_mean | 4.7590 | 6.2687 | 0.4730 | 26065 |
| 2025-26 | pts | all rows | baseline last5_mean | 4.8243 | 6.3346 | 0.4619 | 26065 |
| 2024-25 | pts | starter out | direct | 4.5161 | 5.8621 | 0.5451 | 16058 |
| 2024-25 | pts | starter out | pipeline (min x rate, usage fed) | 4.5188 | 5.8863 | 0.5413 | 16058 |
| 2024-25 | pts | starter out | baseline last10_mean | 4.7516 | 6.2103 | 0.4894 | 16058 |
| 2024-25 | pts | starter out | baseline season_mean | 4.7726 | 6.2894 | 0.4764 | 16058 |
| 2024-25 | pts | starter out | baseline last5_mean | 4.8736 | 6.3921 | 0.4591 | 16058 |
| 2025-26 | pts | starter out | direct | 4.5216 | 5.8929 | 0.5343 | 20472 |
| 2025-26 | pts | starter out | pipeline (min x rate, usage fed) | 4.5236 | 5.9146 | 0.5309 | 20472 |
| 2025-26 | pts | starter out | baseline last10_mean | 4.7152 | 6.2137 | 0.4822 | 20472 |
| 2025-26 | pts | starter out | baseline season_mean | 4.7933 | 6.3375 | 0.4614 | 20472 |
| 2025-26 | pts | starter out | baseline last5_mean | 4.8491 | 6.3782 | 0.4544 | 20472 |
| 2024-25 | pts | starter out, player is a backup | pipeline (min x rate, usage fed) | 3.7862 | 4.9234 | 0.2804 | 8731 |
| 2024-25 | pts | starter out, player is a backup | direct | 3.7973 | 4.9120 | 0.2838 | 8731 |
| 2024-25 | pts | starter out, player is a backup | baseline last10_mean | 4.0005 | 5.2917 | 0.1688 | 8731 |
| 2024-25 | pts | starter out, player is a backup | baseline season_mean | 4.0380 | 5.3727 | 0.1431 | 8731 |
| 2024-25 | pts | starter out, player is a backup | baseline last5_mean | 4.0741 | 5.3801 | 0.1408 | 8731 |
| 2025-26 | pts | starter out, player is a backup | pipeline (min x rate, usage fed) | 3.8500 | 4.9800 | 0.2901 | 11346 |
| 2025-26 | pts | starter out, player is a backup | direct | 3.8583 | 4.9728 | 0.2922 | 11346 |
| 2025-26 | pts | starter out, player is a backup | baseline last10_mean | 4.0246 | 5.3399 | 0.1838 | 11346 |
| 2025-26 | pts | starter out, player is a backup | baseline last5_mean | 4.0926 | 5.4258 | 0.1573 | 11346 |
| 2025-26 | pts | starter out, player is a backup | baseline season_mean | 4.1243 | 5.4674 | 0.1444 | 11346 |
| 2024-25 | reb | all rows | direct | 1.8782 | 2.4751 | 0.4869 | 25025 |
| 2024-25 | reb | all rows | pipeline (min x rate, usage fed) | 1.8984 | 2.5040 | 0.4749 | 25025 |
| 2024-25 | reb | all rows | baseline season_mean | 1.9603 | 2.6000 | 0.4339 | 25025 |
| 2024-25 | reb | all rows | baseline last10_mean | 1.9631 | 2.5893 | 0.4385 | 25025 |
| 2024-25 | reb | all rows | baseline last5_mean | 2.0179 | 2.6617 | 0.4067 | 25025 |
| 2025-26 | reb | all rows | direct | 1.8671 | 2.4428 | 0.4567 | 26065 |
| 2025-26 | reb | all rows | pipeline (min x rate, usage fed) | 1.8976 | 2.4779 | 0.4410 | 26065 |
| 2025-26 | reb | all rows | baseline season_mean | 1.9405 | 2.5739 | 0.3968 | 26065 |
| 2025-26 | reb | all rows | baseline last10_mean | 1.9422 | 2.5617 | 0.4025 | 26065 |
| 2025-26 | reb | all rows | baseline last5_mean | 1.9933 | 2.6392 | 0.3659 | 26065 |
| 2024-25 | reb | starter out | direct | 1.9061 | 2.4918 | 0.4580 | 16058 |
| 2024-25 | reb | starter out | pipeline (min x rate, usage fed) | 1.9250 | 2.5167 | 0.4471 | 16058 |
| 2024-25 | reb | starter out | baseline season_mean | 1.9804 | 2.6316 | 0.3955 | 16058 |
| 2024-25 | reb | starter out | baseline last10_mean | 1.9820 | 2.6108 | 0.4050 | 16058 |
| 2024-25 | reb | starter out | baseline last5_mean | 2.0439 | 2.6852 | 0.3706 | 16058 |
| 2025-26 | reb | starter out | direct | 1.8904 | 2.4678 | 0.4437 | 20472 |
| 2025-26 | reb | starter out | pipeline (min x rate, usage fed) | 1.9218 | 2.5042 | 0.4271 | 20472 |
| 2025-26 | reb | starter out | baseline season_mean | 1.9564 | 2.6044 | 0.3804 | 20472 |
| 2025-26 | reb | starter out | baseline last10_mean | 1.9564 | 2.5870 | 0.3886 | 20472 |
| 2025-26 | reb | starter out | baseline last5_mean | 2.0063 | 2.6591 | 0.3541 | 20472 |
| 2024-25 | reb | starter out, player is a backup | direct | 1.7668 | 2.3385 | 0.3543 | 8731 |
| 2024-25 | reb | starter out, player is a backup | pipeline (min x rate, usage fed) | 1.7799 | 2.3405 | 0.3532 | 8731 |
| 2024-25 | reb | starter out, player is a backup | baseline last10_mean | 1.8481 | 2.4881 | 0.2690 | 8731 |
| 2024-25 | reb | starter out, player is a backup | baseline season_mean | 1.8533 | 2.5053 | 0.2589 | 8731 |
| 2024-25 | reb | starter out, player is a backup | baseline last5_mean | 1.8908 | 2.5299 | 0.2443 | 8731 |
| 2025-26 | reb | starter out, player is a backup | direct | 1.7581 | 2.3189 | 0.3596 | 11346 |
| 2025-26 | reb | starter out, player is a backup | pipeline (min x rate, usage fed) | 1.7838 | 2.3343 | 0.3510 | 11346 |
| 2025-26 | reb | starter out, player is a backup | baseline last10_mean | 1.8226 | 2.4608 | 0.2788 | 11346 |
| 2025-26 | reb | starter out, player is a backup | baseline season_mean | 1.8380 | 2.4913 | 0.2608 | 11346 |
| 2025-26 | reb | starter out, player is a backup | baseline last5_mean | 1.8591 | 2.5148 | 0.2468 | 11346 |

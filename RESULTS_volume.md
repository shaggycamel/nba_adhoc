# Volume projection

Minutes, points, rebounds and assists on the usage project's folds.
Each model is fitted once per fold and predicts the whole validation
season; slices are taken from those predictions. The usage prediction
fed to the rate models (`usg_hat`) is out-of-fold.

`+context+alloc` adds opponent and own-team rolling form, the minutes
allocation implied by the healthy rotation, and each player's measured
minutes response to a starter sitting.

| fold | target | slice | method | mae | rmse | r2 | n |
|---|---|---|---|---|---|---|---|
| 2024-25 | ast | starter out | lightgbm pipeline base | 1.3203 | 1.7910 | 0.5151 | 16058 |
| 2024-25 | ast | starter out | ridge direct base | 1.3213 | 1.7839 | 0.5190 | 16058 |
| 2024-25 | ast | starter out | lightgbm pipeline +context+alloc | 1.3221 | 1.7884 | 0.5165 | 16058 |
| 2024-25 | ast | starter out | ridge direct +context+alloc | 1.3222 | 1.7799 | 0.5211 | 16058 |
| 2024-25 | ast | starter out | lightgbm direct base | 1.3230 | 1.7918 | 0.5147 | 16058 |
| 2024-25 | ast | starter out | lightgbm direct +context+alloc | 1.3234 | 1.7913 | 0.5150 | 16058 |
| 2024-25 | ast | starter out | ridge pipeline base | 1.3253 | 1.7901 | 0.5156 | 16058 |
| 2024-25 | ast | starter out | ridge pipeline +context+alloc | 1.3256 | 1.7858 | 0.5179 | 16058 |
| 2024-25 | ast | starter out | baseline last10 | 1.3693 | 1.8656 | 0.4739 | 16058 |
| 2025-26 | ast | starter out | lightgbm pipeline base | 1.3432 | 1.8176 | 0.4984 | 20472 |
| 2025-26 | ast | starter out | ridge direct +context+alloc | 1.3441 | 1.8129 | 0.5010 | 20472 |
| 2025-26 | ast | starter out | ridge direct base | 1.3445 | 1.8151 | 0.4998 | 20472 |
| 2025-26 | ast | starter out | ridge pipeline base | 1.3447 | 1.8171 | 0.4987 | 20472 |
| 2025-26 | ast | starter out | ridge pipeline +context+alloc | 1.3452 | 1.8129 | 0.5010 | 20472 |
| 2025-26 | ast | starter out | lightgbm pipeline +context+alloc | 1.3458 | 1.8144 | 0.5001 | 20472 |
| 2025-26 | ast | starter out | lightgbm direct base | 1.3518 | 1.8210 | 0.4965 | 20472 |
| 2025-26 | ast | starter out | lightgbm direct +context+alloc | 1.3537 | 1.8225 | 0.4957 | 20472 |
| 2025-26 | ast | starter out | baseline last10 | 1.3849 | 1.8947 | 0.4550 | 20472 |
| 2024-25 | ast | starter out, player is a backup | lightgbm pipeline base | 1.0799 | 1.4731 | 0.3144 | 8731 |
| 2024-25 | ast | starter out, player is a backup | lightgbm direct +context+alloc | 1.0811 | 1.4744 | 0.3132 | 8731 |
| 2024-25 | ast | starter out, player is a backup | lightgbm pipeline +context+alloc | 1.0821 | 1.4712 | 0.3161 | 8731 |
| 2024-25 | ast | starter out, player is a backup | lightgbm direct base | 1.0856 | 1.4781 | 0.3098 | 8731 |
| 2024-25 | ast | starter out, player is a backup | ridge pipeline base | 1.0862 | 1.4710 | 0.3164 | 8731 |
| 2024-25 | ast | starter out, player is a backup | ridge direct base | 1.0863 | 1.4759 | 0.3118 | 8731 |
| 2024-25 | ast | starter out, player is a backup | ridge pipeline +context+alloc | 1.0877 | 1.4672 | 0.3199 | 8731 |
| 2024-25 | ast | starter out, player is a backup | ridge direct +context+alloc | 1.0884 | 1.4719 | 0.3155 | 8731 |
| 2024-25 | ast | starter out, player is a backup | baseline last10 | 1.1150 | 1.5585 | 0.2326 | 8731 |
| 2025-26 | ast | starter out, player is a backup | lightgbm pipeline base | 1.1155 | 1.5163 | 0.3432 | 11346 |
| 2025-26 | ast | starter out, player is a backup | lightgbm pipeline +context+alloc | 1.1205 | 1.5163 | 0.3432 | 11346 |
| 2025-26 | ast | starter out, player is a backup | ridge direct base | 1.1214 | 1.5214 | 0.3387 | 11346 |
| 2025-26 | ast | starter out, player is a backup | ridge pipeline base | 1.1218 | 1.5192 | 0.3406 | 11346 |
| 2025-26 | ast | starter out, player is a backup | lightgbm direct +context+alloc | 1.1235 | 1.5219 | 0.3383 | 11346 |
| 2025-26 | ast | starter out, player is a backup | lightgbm direct base | 1.1240 | 1.5188 | 0.3410 | 11346 |
| 2025-26 | ast | starter out, player is a backup | ridge direct +context+alloc | 1.1243 | 1.5230 | 0.3373 | 11346 |
| 2025-26 | ast | starter out, player is a backup | ridge pipeline +context+alloc | 1.1253 | 1.5193 | 0.3405 | 11346 |
| 2025-26 | ast | starter out, player is a backup | baseline last10 | 1.1504 | 1.6078 | 0.2615 | 11346 |
| 2024-25 | min | starter out | lightgbm base | 4.6806 | 6.0508 | 0.6589 | 16058 |
| 2024-25 | min | starter out | lightgbm +context+alloc | 4.6807 | 6.0362 | 0.6606 | 16058 |
| 2024-25 | min | starter out | ridge +context+alloc | 4.7095 | 6.0770 | 0.6560 | 16058 |
| 2024-25 | min | starter out | ridge base | 4.7412 | 6.1236 | 0.6506 | 16058 |
| 2024-25 | min | starter out | baseline last5 | 5.1842 | 6.8516 | 0.5627 | 16058 |
| 2024-25 | min | starter out | baseline last10 | 5.2572 | 6.9237 | 0.5534 | 16058 |
| 2024-25 | min | starter out | expected_min_alloc (no model) | 5.3475 | 6.8824 | 0.5587 | 16058 |
| 2025-26 | min | starter out | lightgbm base | 4.7074 | 6.0828 | 0.6388 | 20472 |
| 2025-26 | min | starter out | lightgbm +context+alloc | 4.7103 | 6.0758 | 0.6397 | 20472 |
| 2025-26 | min | starter out | ridge +context+alloc | 4.7487 | 6.1357 | 0.6325 | 20472 |
| 2025-26 | min | starter out | ridge base | 4.7700 | 6.1640 | 0.6291 | 20472 |
| 2025-26 | min | starter out | baseline last5 | 5.2206 | 6.8786 | 0.5382 | 20472 |
| 2025-26 | min | starter out | baseline last10 | 5.3114 | 6.9771 | 0.5248 | 20472 |
| 2025-26 | min | starter out | expected_min_alloc (no model) | 5.3496 | 6.9234 | 0.5321 | 20472 |
| 2024-25 | min | starter out, player is a backup | lightgbm +context+alloc | 5.0698 | 6.4235 | 0.4779 | 8731 |
| 2024-25 | min | starter out, player is a backup | lightgbm base | 5.0776 | 6.4454 | 0.4743 | 8731 |
| 2024-25 | min | starter out, player is a backup | ridge +context+alloc | 5.1155 | 6.4523 | 0.4732 | 8731 |
| 2024-25 | min | starter out, player is a backup | ridge base | 5.1252 | 6.4770 | 0.4691 | 8731 |
| 2024-25 | min | starter out, player is a backup | expected_min_alloc (no model) | 5.5754 | 7.2085 | 0.3424 | 8731 |
| 2024-25 | min | starter out, player is a backup | baseline last5 | 5.6817 | 7.4081 | 0.3055 | 8731 |
| 2024-25 | min | starter out, player is a backup | baseline last10 | 5.8566 | 7.5817 | 0.2726 | 8731 |
| 2025-26 | min | starter out, player is a backup | lightgbm +context+alloc | 5.1061 | 6.4434 | 0.4658 | 11346 |
| 2025-26 | min | starter out, player is a backup | lightgbm base | 5.1085 | 6.4541 | 0.4641 | 11346 |
| 2025-26 | min | starter out, player is a backup | ridge +context+alloc | 5.1393 | 6.4939 | 0.4574 | 11346 |
| 2025-26 | min | starter out, player is a backup | ridge base | 5.1434 | 6.5051 | 0.4556 | 11346 |
| 2025-26 | min | starter out, player is a backup | expected_min_alloc (no model) | 5.6105 | 7.2074 | 0.3316 | 11346 |
| 2025-26 | min | starter out, player is a backup | baseline last5 | 5.7314 | 7.4561 | 0.2847 | 11346 |
| 2025-26 | min | starter out, player is a backup | baseline last10 | 5.9011 | 7.6319 | 0.2506 | 11346 |
| 2024-25 | pts | starter out | ridge pipeline +context+alloc | 4.5123 | 5.8713 | 0.5437 | 16058 |
| 2024-25 | pts | starter out | ridge direct base | 4.5161 | 5.8621 | 0.5451 | 16058 |
| 2024-25 | pts | starter out | ridge direct +context+alloc | 4.5170 | 5.8521 | 0.5466 | 16058 |
| 2024-25 | pts | starter out | ridge pipeline base | 4.5188 | 5.8863 | 0.5413 | 16058 |
| 2024-25 | pts | starter out | lightgbm pipeline +context+alloc | 4.5217 | 5.8972 | 0.5396 | 16058 |
| 2024-25 | pts | starter out | lightgbm pipeline base | 4.5275 | 5.9093 | 0.5377 | 16058 |
| 2024-25 | pts | starter out | lightgbm direct +context+alloc | 4.5467 | 5.9020 | 0.5389 | 16058 |
| 2024-25 | pts | starter out | lightgbm direct base | 4.5522 | 5.9040 | 0.5386 | 16058 |
| 2024-25 | pts | starter out | baseline last10 | 4.7516 | 6.2103 | 0.4894 | 16058 |
| 2025-26 | pts | starter out | lightgbm pipeline base | 4.4987 | 5.9011 | 0.5330 | 20472 |
| 2025-26 | pts | starter out | lightgbm pipeline +context+alloc | 4.5010 | 5.8803 | 0.5363 | 20472 |
| 2025-26 | pts | starter out | lightgbm direct +context+alloc | 4.5095 | 5.8863 | 0.5353 | 20472 |
| 2025-26 | pts | starter out | lightgbm direct base | 4.5100 | 5.8929 | 0.5343 | 20472 |
| 2025-26 | pts | starter out | ridge pipeline +context+alloc | 4.5210 | 5.8961 | 0.5338 | 20472 |
| 2025-26 | pts | starter out | ridge direct base | 4.5216 | 5.8929 | 0.5343 | 20472 |
| 2025-26 | pts | starter out | ridge pipeline base | 4.5236 | 5.9146 | 0.5309 | 20472 |
| 2025-26 | pts | starter out | ridge direct +context+alloc | 4.5322 | 5.8844 | 0.5356 | 20472 |
| 2025-26 | pts | starter out | baseline last10 | 4.7152 | 6.2137 | 0.4822 | 20472 |
| 2024-25 | pts | starter out, player is a backup | lightgbm pipeline +context+alloc | 3.7834 | 4.9355 | 0.2769 | 8731 |
| 2024-25 | pts | starter out, player is a backup | ridge pipeline +context+alloc | 3.7834 | 4.9076 | 0.2851 | 8731 |
| 2024-25 | pts | starter out, player is a backup | lightgbm pipeline base | 3.7841 | 4.9460 | 0.2738 | 8731 |
| 2024-25 | pts | starter out, player is a backup | ridge pipeline base | 3.7862 | 4.9234 | 0.2804 | 8731 |
| 2024-25 | pts | starter out, player is a backup | ridge direct base | 3.7973 | 4.9120 | 0.2838 | 8731 |
| 2024-25 | pts | starter out, player is a backup | ridge direct +context+alloc | 3.8054 | 4.9006 | 0.2871 | 8731 |
| 2024-25 | pts | starter out, player is a backup | lightgbm direct +context+alloc | 3.8066 | 4.9277 | 0.2792 | 8731 |
| 2024-25 | pts | starter out, player is a backup | lightgbm direct base | 3.8246 | 4.9388 | 0.2759 | 8731 |
| 2024-25 | pts | starter out, player is a backup | baseline last10 | 4.0005 | 5.2917 | 0.1688 | 8731 |
| 2025-26 | pts | starter out, player is a backup | lightgbm pipeline base | 3.8255 | 4.9854 | 0.2886 | 11346 |
| 2025-26 | pts | starter out, player is a backup | lightgbm pipeline +context+alloc | 3.8311 | 4.9667 | 0.2939 | 11346 |
| 2025-26 | pts | starter out, player is a backup | lightgbm direct +context+alloc | 3.8485 | 4.9709 | 0.2927 | 11346 |
| 2025-26 | pts | starter out, player is a backup | lightgbm direct base | 3.8497 | 4.9771 | 0.2909 | 11346 |
| 2025-26 | pts | starter out, player is a backup | ridge pipeline base | 3.8500 | 4.9800 | 0.2901 | 11346 |
| 2025-26 | pts | starter out, player is a backup | ridge direct base | 3.8583 | 4.9728 | 0.2922 | 11346 |
| 2025-26 | pts | starter out, player is a backup | ridge pipeline +context+alloc | 3.8585 | 4.9684 | 0.2934 | 11346 |
| 2025-26 | pts | starter out, player is a backup | ridge direct +context+alloc | 3.8868 | 4.9736 | 0.2919 | 11346 |
| 2025-26 | pts | starter out, player is a backup | baseline last10 | 4.0246 | 5.3399 | 0.1838 | 11346 |
| 2024-25 | reb | starter out | lightgbm pipeline base | 1.9040 | 2.5041 | 0.4526 | 16058 |
| 2024-25 | reb | starter out | lightgbm direct +context+alloc | 1.9043 | 2.5071 | 0.4513 | 16058 |
| 2024-25 | reb | starter out | lightgbm direct base | 1.9046 | 2.5045 | 0.4524 | 16058 |
| 2024-25 | reb | starter out | ridge direct base | 1.9061 | 2.4918 | 0.4580 | 16058 |
| 2024-25 | reb | starter out | ridge direct +context+alloc | 1.9064 | 2.4881 | 0.4596 | 16058 |
| 2024-25 | reb | starter out | lightgbm pipeline +context+alloc | 1.9072 | 2.5056 | 0.4520 | 16058 |
| 2024-25 | reb | starter out | ridge pipeline +context+alloc | 1.9225 | 2.5126 | 0.4489 | 16058 |
| 2024-25 | reb | starter out | ridge pipeline base | 1.9250 | 2.5167 | 0.4471 | 16058 |
| 2024-25 | reb | starter out | baseline last10 | 1.9820 | 2.6108 | 0.4050 | 16058 |
| 2025-26 | reb | starter out | lightgbm direct +context+alloc | 1.8814 | 2.4622 | 0.4462 | 20472 |
| 2025-26 | reb | starter out | lightgbm direct base | 1.8825 | 2.4661 | 0.4444 | 20472 |
| 2025-26 | reb | starter out | lightgbm pipeline +context+alloc | 1.8847 | 2.4652 | 0.4448 | 20472 |
| 2025-26 | reb | starter out | lightgbm pipeline base | 1.8849 | 2.4701 | 0.4427 | 20472 |
| 2025-26 | reb | starter out | ridge direct base | 1.8904 | 2.4678 | 0.4437 | 20472 |
| 2025-26 | reb | starter out | ridge direct +context+alloc | 1.8909 | 2.4625 | 0.4461 | 20472 |
| 2025-26 | reb | starter out | ridge pipeline +context+alloc | 1.9187 | 2.4998 | 0.4292 | 20472 |
| 2025-26 | reb | starter out | ridge pipeline base | 1.9218 | 2.5042 | 0.4271 | 20472 |
| 2025-26 | reb | starter out | baseline last10 | 1.9564 | 2.5870 | 0.3886 | 20472 |
| 2024-25 | reb | starter out, player is a backup | lightgbm pipeline +context+alloc | 1.7591 | 2.3423 | 0.3522 | 8731 |
| 2024-25 | reb | starter out, player is a backup | lightgbm direct +context+alloc | 1.7596 | 2.3427 | 0.3520 | 8731 |
| 2024-25 | reb | starter out, player is a backup | lightgbm pipeline base | 1.7603 | 2.3481 | 0.3490 | 8731 |
| 2024-25 | reb | starter out, player is a backup | lightgbm direct base | 1.7637 | 2.3458 | 0.3503 | 8731 |
| 2024-25 | reb | starter out, player is a backup | ridge direct base | 1.7668 | 2.3385 | 0.3543 | 8731 |
| 2024-25 | reb | starter out, player is a backup | ridge direct +context+alloc | 1.7680 | 2.3336 | 0.3570 | 8731 |
| 2024-25 | reb | starter out, player is a backup | ridge pipeline +context+alloc | 1.7774 | 2.3364 | 0.3555 | 8731 |
| 2024-25 | reb | starter out, player is a backup | ridge pipeline base | 1.7799 | 2.3405 | 0.3532 | 8731 |
| 2024-25 | reb | starter out, player is a backup | baseline last10 | 1.8481 | 2.4881 | 0.2690 | 8731 |
| 2025-26 | reb | starter out, player is a backup | lightgbm direct base | 1.7497 | 2.3169 | 0.3607 | 11346 |
| 2025-26 | reb | starter out, player is a backup | lightgbm pipeline base | 1.7518 | 2.3246 | 0.3564 | 11346 |
| 2025-26 | reb | starter out, player is a backup | lightgbm direct +context+alloc | 1.7526 | 2.3156 | 0.3614 | 11346 |
| 2025-26 | reb | starter out, player is a backup | lightgbm pipeline +context+alloc | 1.7536 | 2.3174 | 0.3604 | 11346 |
| 2025-26 | reb | starter out, player is a backup | ridge direct base | 1.7581 | 2.3189 | 0.3596 | 11346 |
| 2025-26 | reb | starter out, player is a backup | ridge direct +context+alloc | 1.7615 | 2.3130 | 0.3628 | 11346 |
| 2025-26 | reb | starter out, player is a backup | ridge pipeline +context+alloc | 1.7802 | 2.3280 | 0.3545 | 11346 |
| 2025-26 | reb | starter out, player is a backup | ridge pipeline base | 1.7838 | 2.3343 | 0.3510 | 11346 |
| 2025-26 | reb | starter out, player is a backup | baseline last10 | 1.8226 | 2.4608 | 0.2788 | 11346 |

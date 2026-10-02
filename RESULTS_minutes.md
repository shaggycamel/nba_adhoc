# Minutes and absence duration

`+spells` adds the hazard model's expected remaining absence for the
team's absentees, how many games the team has already played short-handed,
and how recently the focal player returned from an absence of their own.

| fold | target | slice | method | mae | rmse | r2 | n |
|---|---|---|---|---|---|---|---|
| 2024-25 | min | first game of the absence | lightgbm standings (current best) | 4.6657 | 6.0308 | 0.6662 | 12049 |
| 2024-25 | min | first game of the absence | lightgbm +mismatch | 4.6696 | 6.0328 | 0.6660 | 12049 |
| 2024-25 | min | first game of the absence | lightgbm +mismatch+transactions | 4.6703 | 6.0293 | 0.6664 | 12049 |
| 2024-25 | min | first game of the absence | lightgbm +transactions | 4.6778 | 6.0347 | 0.6658 | 12049 |
| 2024-25 | min | first game of the absence | ridge +mismatch | 4.7162 | 6.0780 | 0.6610 | 12049 |
| 2024-25 | min | first game of the absence | ridge standings (current best) | 4.7165 | 6.0782 | 0.6609 | 12049 |
| 2024-25 | min | first game of the absence | ridge +mismatch+transactions | 4.7169 | 6.0793 | 0.6608 | 12049 |
| 2024-25 | min | first game of the absence | ridge +transactions | 4.7171 | 6.0796 | 0.6608 | 12049 |
| 2024-25 | min | first game of the absence | baseline last10 | 5.3035 | 6.9788 | 0.5530 | 12049 |
| 2025-26 | min | first game of the absence | lightgbm +mismatch | 4.7109 | 6.0877 | 0.6283 | 14678 |
| 2025-26 | min | first game of the absence | lightgbm +mismatch+transactions | 4.7133 | 6.0918 | 0.6278 | 14678 |
| 2025-26 | min | first game of the absence | lightgbm standings (current best) | 4.7179 | 6.0872 | 0.6283 | 14678 |
| 2025-26 | min | first game of the absence | lightgbm +transactions | 4.7192 | 6.0967 | 0.6272 | 14678 |
| 2025-26 | min | first game of the absence | ridge +mismatch | 4.7861 | 6.1851 | 0.6163 | 14678 |
| 2025-26 | min | first game of the absence | ridge standings (current best) | 4.7861 | 6.1853 | 0.6163 | 14678 |
| 2025-26 | min | first game of the absence | ridge +mismatch+transactions | 4.7878 | 6.1872 | 0.6160 | 14678 |
| 2025-26 | min | first game of the absence | ridge +transactions | 4.7878 | 6.1873 | 0.6160 | 14678 |
| 2025-26 | min | first game of the absence | baseline last10 | 5.3922 | 7.1123 | 0.4926 | 14678 |
| 2024-25 | min | starter out | lightgbm standings (current best) | 4.6591 | 6.0252 | 0.6618 | 16058 |
| 2024-25 | min | starter out | lightgbm +mismatch | 4.6650 | 6.0250 | 0.6618 | 16058 |
| 2024-25 | min | starter out | lightgbm +transactions | 4.6684 | 6.0294 | 0.6613 | 16058 |
| 2024-25 | min | starter out | lightgbm +mismatch+transactions | 4.6684 | 6.0282 | 0.6615 | 16058 |
| 2024-25 | min | starter out | ridge +mismatch | 4.7195 | 6.0885 | 0.6546 | 16058 |
| 2024-25 | min | starter out | ridge standings (current best) | 4.7199 | 6.0888 | 0.6546 | 16058 |
| 2024-25 | min | starter out | ridge +mismatch+transactions | 4.7216 | 6.0907 | 0.6544 | 16058 |
| 2024-25 | min | starter out | ridge +transactions | 4.7219 | 6.0910 | 0.6544 | 16058 |
| 2024-25 | min | starter out | baseline last10 | 5.2572 | 6.9237 | 0.5534 | 16058 |
| 2025-26 | min | starter out | lightgbm +mismatch | 4.6914 | 6.0563 | 0.6420 | 20472 |
| 2025-26 | min | starter out | lightgbm +mismatch+transactions | 4.6931 | 6.0601 | 0.6415 | 20472 |
| 2025-26 | min | starter out | lightgbm +transactions | 4.7006 | 6.0662 | 0.6408 | 20472 |
| 2025-26 | min | starter out | lightgbm standings (current best) | 4.7012 | 6.0608 | 0.6414 | 20472 |
| 2025-26 | min | starter out | ridge +mismatch | 4.7486 | 6.1331 | 0.6328 | 20472 |
| 2025-26 | min | starter out | ridge standings (current best) | 4.7488 | 6.1332 | 0.6328 | 20472 |
| 2025-26 | min | starter out | ridge +mismatch+transactions | 4.7508 | 6.1362 | 0.6325 | 20472 |
| 2025-26 | min | starter out | ridge +transactions | 4.7510 | 6.1363 | 0.6325 | 20472 |
| 2025-26 | min | starter out | baseline last10 | 5.3114 | 6.9771 | 0.5248 | 20472 |
| 2024-25 | min | starter out, player is a backup | lightgbm standings (current best) | 5.0527 | 6.4178 | 0.4788 | 8731 |
| 2024-25 | min | starter out, player is a backup | lightgbm +transactions | 5.0582 | 6.4201 | 0.4784 | 8731 |
| 2024-25 | min | starter out, player is a backup | lightgbm +mismatch | 5.0591 | 6.4184 | 0.4787 | 8731 |
| 2024-25 | min | starter out, player is a backup | lightgbm +mismatch+transactions | 5.0667 | 6.4223 | 0.4780 | 8731 |
| 2024-25 | min | starter out, player is a backup | ridge +mismatch | 5.1207 | 6.4546 | 0.4728 | 8731 |
| 2024-25 | min | starter out, player is a backup | ridge standings (current best) | 5.1211 | 6.4548 | 0.4728 | 8731 |
| 2024-25 | min | starter out, player is a backup | ridge +mismatch+transactions | 5.1241 | 6.4575 | 0.4723 | 8731 |
| 2024-25 | min | starter out, player is a backup | ridge +transactions | 5.1245 | 6.4576 | 0.4723 | 8731 |
| 2024-25 | min | starter out, player is a backup | baseline last10 | 5.8566 | 7.5817 | 0.2726 | 8731 |
| 2025-26 | min | starter out, player is a backup | lightgbm +mismatch+transactions | 5.0968 | 6.4409 | 0.4663 | 11346 |
| 2025-26 | min | starter out, player is a backup | lightgbm +mismatch | 5.0977 | 6.4399 | 0.4664 | 11346 |
| 2025-26 | min | starter out, player is a backup | lightgbm standings (current best) | 5.1034 | 6.4398 | 0.4664 | 11346 |
| 2025-26 | min | starter out, player is a backup | lightgbm +transactions | 5.1071 | 6.4464 | 0.4653 | 11346 |
| 2025-26 | min | starter out, player is a backup | ridge standings (current best) | 5.1350 | 6.4877 | 0.4585 | 11346 |
| 2025-26 | min | starter out, player is a backup | ridge +mismatch | 5.1352 | 6.4879 | 0.4584 | 11346 |
| 2025-26 | min | starter out, player is a backup | ridge +transactions | 5.1397 | 6.4914 | 0.4578 | 11346 |
| 2025-26 | min | starter out, player is a backup | ridge +mismatch+transactions | 5.1398 | 6.4916 | 0.4578 | 11346 |
| 2025-26 | min | starter out, player is a backup | baseline last10 | 5.9011 | 7.6319 | 0.2506 | 11346 |
| 2024-25 | pts | first game of the absence | ridge pipeline +mismatch | 4.4807 | 5.8427 | 0.5472 | 12049 |
| 2024-25 | pts | first game of the absence | ridge pipeline standings (current best) | 4.4811 | 5.8424 | 0.5473 | 12049 |
| 2024-25 | pts | first game of the absence | ridge pipeline +mismatch+transactions | 4.4814 | 5.8436 | 0.5471 | 12049 |
| 2024-25 | pts | first game of the absence | ridge pipeline +transactions | 4.4819 | 5.8433 | 0.5471 | 12049 |
| 2024-25 | pts | first game of the absence | lightgbm pipeline +mismatch | 4.4870 | 5.8482 | 0.5464 | 12049 |
| 2024-25 | pts | first game of the absence | lightgbm pipeline standings (current best) | 4.4882 | 5.8507 | 0.5460 | 12049 |
| 2024-25 | pts | first game of the absence | lightgbm pipeline +transactions | 4.4897 | 5.8510 | 0.5459 | 12049 |
| 2024-25 | pts | first game of the absence | lightgbm pipeline +mismatch+transactions | 4.4906 | 5.8532 | 0.5456 | 12049 |
| 2025-26 | pts | first game of the absence | lightgbm pipeline +mismatch | 4.5056 | 5.9020 | 0.5274 | 14678 |
| 2025-26 | pts | first game of the absence | lightgbm pipeline +transactions | 4.5090 | 5.9029 | 0.5272 | 14678 |
| 2025-26 | pts | first game of the absence | lightgbm pipeline +mismatch+transactions | 4.5120 | 5.9067 | 0.5266 | 14678 |
| 2025-26 | pts | first game of the absence | lightgbm pipeline standings (current best) | 4.5134 | 5.9039 | 0.5271 | 14678 |
| 2025-26 | pts | first game of the absence | ridge pipeline standings (current best) | 4.5207 | 5.9132 | 0.5256 | 14678 |
| 2025-26 | pts | first game of the absence | ridge pipeline +transactions | 4.5215 | 5.9136 | 0.5255 | 14678 |
| 2025-26 | pts | first game of the absence | ridge pipeline +mismatch | 4.5223 | 5.9157 | 0.5252 | 14678 |
| 2025-26 | pts | first game of the absence | ridge pipeline +mismatch+transactions | 4.5231 | 5.9162 | 0.5251 | 14678 |
| 2024-25 | pts | starter out | ridge pipeline +mismatch | 4.5141 | 5.8747 | 0.5431 | 16058 |
| 2024-25 | pts | starter out | ridge pipeline standings (current best) | 4.5144 | 5.8745 | 0.5432 | 16058 |
| 2024-25 | pts | starter out | ridge pipeline +mismatch+transactions | 4.5161 | 5.8774 | 0.5427 | 16058 |
| 2024-25 | pts | starter out | ridge pipeline +transactions | 4.5165 | 5.8772 | 0.5427 | 16058 |
| 2024-25 | pts | starter out | lightgbm pipeline standings (current best) | 4.5167 | 5.8990 | 0.5393 | 16058 |
| 2024-25 | pts | starter out | lightgbm pipeline +mismatch | 4.5167 | 5.8970 | 0.5397 | 16058 |
| 2024-25 | pts | starter out | lightgbm pipeline +transactions | 4.5204 | 5.9012 | 0.5390 | 16058 |
| 2024-25 | pts | starter out | lightgbm pipeline +mismatch+transactions | 4.5210 | 5.9020 | 0.5389 | 16058 |
| 2025-26 | pts | starter out | lightgbm pipeline +mismatch | 4.4896 | 5.8742 | 0.5372 | 20472 |
| 2025-26 | pts | starter out | lightgbm pipeline +transactions | 4.4937 | 5.8756 | 0.5370 | 20472 |
| 2025-26 | pts | starter out | lightgbm pipeline +mismatch+transactions | 4.4964 | 5.8784 | 0.5366 | 20472 |
| 2025-26 | pts | starter out | lightgbm pipeline standings (current best) | 4.4975 | 5.8748 | 0.5371 | 20472 |
| 2025-26 | pts | starter out | ridge pipeline standings (current best) | 4.5152 | 5.8946 | 0.5340 | 20472 |
| 2025-26 | pts | starter out | ridge pipeline +transactions | 4.5159 | 5.8954 | 0.5339 | 20472 |
| 2025-26 | pts | starter out | ridge pipeline +mismatch | 4.5163 | 5.8967 | 0.5337 | 20472 |
| 2025-26 | pts | starter out | ridge pipeline +mismatch+transactions | 4.5170 | 5.8975 | 0.5336 | 20472 |
| 2024-25 | pts | starter out, player is a backup | lightgbm pipeline +mismatch | 3.7791 | 4.9433 | 0.2746 | 8731 |
| 2024-25 | pts | starter out, player is a backup | lightgbm pipeline standings (current best) | 3.7828 | 4.9431 | 0.2747 | 8731 |
| 2024-25 | pts | starter out, player is a backup | ridge pipeline +mismatch | 3.7840 | 4.9090 | 0.2847 | 8731 |
| 2024-25 | pts | starter out, player is a backup | lightgbm pipeline +transactions | 3.7842 | 4.9445 | 0.2743 | 8731 |
| 2024-25 | pts | starter out, player is a backup | ridge pipeline standings (current best) | 3.7842 | 4.9087 | 0.2847 | 8731 |
| 2024-25 | pts | starter out, player is a backup | ridge pipeline +mismatch+transactions | 3.7862 | 4.9106 | 0.2842 | 8731 |
| 2024-25 | pts | starter out, player is a backup | ridge pipeline +transactions | 3.7864 | 4.9103 | 0.2843 | 8731 |
| 2024-25 | pts | starter out, player is a backup | lightgbm pipeline +mismatch+transactions | 3.7872 | 4.9448 | 0.2742 | 8731 |
| 2025-26 | pts | starter out, player is a backup | lightgbm pipeline +mismatch | 3.8232 | 4.9636 | 0.2948 | 11346 |
| 2025-26 | pts | starter out, player is a backup | lightgbm pipeline +mismatch+transactions | 3.8251 | 4.9582 | 0.2963 | 11346 |
| 2025-26 | pts | starter out, player is a backup | lightgbm pipeline +transactions | 3.8259 | 4.9615 | 0.2954 | 11346 |
| 2025-26 | pts | starter out, player is a backup | lightgbm pipeline standings (current best) | 3.8337 | 4.9641 | 0.2946 | 11346 |
| 2025-26 | pts | starter out, player is a backup | ridge pipeline standings (current best) | 3.8497 | 4.9653 | 0.2943 | 11346 |
| 2025-26 | pts | starter out, player is a backup | ridge pipeline +mismatch | 3.8498 | 4.9658 | 0.2942 | 11346 |
| 2025-26 | pts | starter out, player is a backup | ridge pipeline +transactions | 3.8508 | 4.9663 | 0.2940 | 11346 |
| 2025-26 | pts | starter out, player is a backup | ridge pipeline +mismatch+transactions | 3.8508 | 4.9668 | 0.2939 | 11346 |

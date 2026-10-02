# Minutes and absence duration

`+spells` adds the hazard model's expected remaining absence for the
team's absentees, how many games the team has already played short-handed,
and how recently the focal player returned from an absence of their own.

| fold | target | slice | method | mae | rmse | r2 | n |
|---|---|---|---|---|---|---|---|
| 2024-25 | min | first game of the absence | lightgbm no spells | 4.6833 | 6.0393 | 0.6653 | 12049 |
| 2024-25 | min | first game of the absence | lightgbm +spells | 4.6917 | 6.0423 | 0.6649 | 12049 |
| 2024-25 | min | first game of the absence | ridge no spells | 4.7138 | 6.0739 | 0.6614 | 12049 |
| 2024-25 | min | first game of the absence | ridge +spells | 4.7167 | 6.0781 | 0.6609 | 12049 |
| 2024-25 | min | first game of the absence | baseline last10 | 5.3035 | 6.9788 | 0.5530 | 12049 |
| 2025-26 | min | first game of the absence | lightgbm +spells | 4.7312 | 6.1063 | 0.6260 | 14678 |
| 2025-26 | min | first game of the absence | lightgbm no spells | 4.7349 | 6.1140 | 0.6251 | 14678 |
| 2025-26 | min | first game of the absence | ridge no spells | 4.7817 | 6.1822 | 0.6167 | 14678 |
| 2025-26 | min | first game of the absence | ridge +spells | 4.7853 | 6.1858 | 0.6162 | 14678 |
| 2025-26 | min | first game of the absence | baseline last10 | 5.3922 | 7.1123 | 0.4926 | 14678 |
| 2024-25 | min | starter out | lightgbm no spells | 4.6807 | 6.0362 | 0.6606 | 16058 |
| 2024-25 | min | starter out | lightgbm +spells | 4.6872 | 6.0374 | 0.6604 | 16058 |
| 2024-25 | min | starter out | ridge no spells | 4.7095 | 6.0770 | 0.6560 | 16058 |
| 2024-25 | min | starter out | ridge +spells | 4.7163 | 6.0856 | 0.6550 | 16058 |
| 2024-25 | min | starter out | baseline last10 | 5.2572 | 6.9237 | 0.5534 | 16058 |
| 2025-26 | min | starter out | lightgbm no spells | 4.7103 | 6.0758 | 0.6397 | 20472 |
| 2025-26 | min | starter out | lightgbm +spells | 4.7108 | 6.0753 | 0.6397 | 20472 |
| 2025-26 | min | starter out | ridge no spells | 4.7487 | 6.1357 | 0.6325 | 20472 |
| 2025-26 | min | starter out | ridge +spells | 4.7500 | 6.1364 | 0.6324 | 20472 |
| 2025-26 | min | starter out | baseline last10 | 5.3114 | 6.9771 | 0.5248 | 20472 |
| 2024-25 | min | starter out, player is a backup | lightgbm no spells | 5.0698 | 6.4235 | 0.4779 | 8731 |
| 2024-25 | min | starter out, player is a backup | lightgbm +spells | 5.0823 | 6.4305 | 0.4767 | 8731 |
| 2024-25 | min | starter out, player is a backup | ridge no spells | 5.1155 | 6.4523 | 0.4732 | 8731 |
| 2024-25 | min | starter out, player is a backup | ridge +spells | 5.1243 | 6.4559 | 0.4726 | 8731 |
| 2024-25 | min | starter out, player is a backup | baseline last10 | 5.8566 | 7.5817 | 0.2726 | 8731 |
| 2025-26 | min | starter out, player is a backup | lightgbm no spells | 5.1061 | 6.4434 | 0.4658 | 11346 |
| 2025-26 | min | starter out, player is a backup | lightgbm +spells | 5.1126 | 6.4507 | 0.4646 | 11346 |
| 2025-26 | min | starter out, player is a backup | ridge no spells | 5.1393 | 6.4939 | 0.4574 | 11346 |
| 2025-26 | min | starter out, player is a backup | ridge +spells | 5.1400 | 6.4928 | 0.4576 | 11346 |
| 2025-26 | min | starter out, player is a backup | baseline last10 | 5.9011 | 7.6319 | 0.2506 | 11346 |
| 2024-25 | pts | first game of the absence | ridge pipeline no spells | 4.4787 | 5.8398 | 0.5477 | 12049 |
| 2024-25 | pts | first game of the absence | ridge pipeline +spells | 4.4796 | 5.8396 | 0.5477 | 12049 |
| 2024-25 | pts | first game of the absence | lightgbm pipeline no spells | 4.4894 | 5.8514 | 0.5459 | 12049 |
| 2024-25 | pts | first game of the absence | lightgbm pipeline +spells | 4.4911 | 5.8536 | 0.5455 | 12049 |
| 2025-26 | pts | first game of the absence | lightgbm pipeline no spells | 4.5174 | 5.9087 | 0.5263 | 14678 |
| 2025-26 | pts | first game of the absence | lightgbm pipeline +spells | 4.5212 | 5.9175 | 0.5249 | 14678 |
| 2025-26 | pts | first game of the absence | ridge pipeline +spells | 4.5225 | 5.9133 | 0.5256 | 14678 |
| 2025-26 | pts | first game of the absence | ridge pipeline no spells | 4.5233 | 5.9131 | 0.5256 | 14678 |
| 2024-25 | pts | starter out | ridge pipeline no spells | 4.5123 | 5.8713 | 0.5437 | 16058 |
| 2024-25 | pts | starter out | ridge pipeline +spells | 4.5123 | 5.8707 | 0.5438 | 16058 |
| 2024-25 | pts | starter out | lightgbm pipeline no spells | 4.5217 | 5.8972 | 0.5396 | 16058 |
| 2024-25 | pts | starter out | lightgbm pipeline +spells | 4.5239 | 5.9031 | 0.5387 | 16058 |
| 2025-26 | pts | starter out | lightgbm pipeline no spells | 4.5010 | 5.8803 | 0.5363 | 20472 |
| 2025-26 | pts | starter out | lightgbm pipeline +spells | 4.5016 | 5.8873 | 0.5352 | 20472 |
| 2025-26 | pts | starter out | ridge pipeline +spells | 4.5189 | 5.8959 | 0.5338 | 20472 |
| 2025-26 | pts | starter out | ridge pipeline no spells | 4.5210 | 5.8961 | 0.5338 | 20472 |
| 2024-25 | pts | starter out, player is a backup | lightgbm pipeline +spells | 3.7827 | 4.9347 | 0.2771 | 8731 |
| 2024-25 | pts | starter out, player is a backup | lightgbm pipeline no spells | 3.7834 | 4.9355 | 0.2769 | 8731 |
| 2024-25 | pts | starter out, player is a backup | ridge pipeline no spells | 3.7834 | 4.9076 | 0.2851 | 8731 |
| 2024-25 | pts | starter out, player is a backup | ridge pipeline +spells | 3.7844 | 4.9069 | 0.2853 | 8731 |
| 2025-26 | pts | starter out, player is a backup | lightgbm pipeline no spells | 3.8311 | 4.9667 | 0.2939 | 11346 |
| 2025-26 | pts | starter out, player is a backup | lightgbm pipeline +spells | 3.8359 | 4.9729 | 0.2921 | 11346 |
| 2025-26 | pts | starter out, player is a backup | ridge pipeline +spells | 3.8558 | 4.9679 | 0.2936 | 11346 |
| 2025-26 | pts | starter out, player is a backup | ridge pipeline no spells | 3.8585 | 4.9684 | 0.2934 | 11346 |

# Minutes and absence duration

`+spells` adds the hazard model's expected remaining absence for the
team's absentees, how many games the team has already played short-handed,
and how recently the focal player returned from an absence of their own.

| fold | target | slice | method | mae | rmse | r2 | n |
|---|---|---|---|---|---|---|---|
| 2024-25 | min | first game of the absence | lightgbm +standings | 4.6682 | 6.0252 | 0.6668 | 12049 |
| 2024-25 | min | first game of the absence | lightgbm +attrs+standings | 4.6684 | 6.0259 | 0.6667 | 12049 |
| 2024-25 | min | first game of the absence | lightgbm +attributes | 4.6710 | 6.0255 | 0.6668 | 12049 |
| 2024-25 | min | first game of the absence | lightgbm no spells | 4.6833 | 6.0393 | 0.6653 | 12049 |
| 2024-25 | min | first game of the absence | lightgbm +spells | 4.6917 | 6.0423 | 0.6649 | 12049 |
| 2024-25 | min | first game of the absence | ridge +attrs+standings | 4.7068 | 6.0685 | 0.6620 | 12049 |
| 2024-25 | min | first game of the absence | ridge +attributes | 4.7081 | 6.0692 | 0.6619 | 12049 |
| 2024-25 | min | first game of the absence | ridge no spells | 4.7138 | 6.0739 | 0.6614 | 12049 |
| 2024-25 | min | first game of the absence | ridge +standings | 4.7165 | 6.0782 | 0.6609 | 12049 |
| 2024-25 | min | first game of the absence | ridge +spells | 4.7173 | 6.0787 | 0.6609 | 12049 |
| 2024-25 | min | first game of the absence | baseline last10 | 5.3035 | 6.9788 | 0.5530 | 12049 |
| 2025-26 | min | first game of the absence | lightgbm +standings | 4.7159 | 6.0953 | 0.6274 | 14678 |
| 2025-26 | min | first game of the absence | lightgbm +attrs+standings | 4.7180 | 6.0902 | 0.6280 | 14678 |
| 2025-26 | min | first game of the absence | lightgbm +attributes | 4.7256 | 6.0965 | 0.6272 | 14678 |
| 2025-26 | min | first game of the absence | lightgbm no spells | 4.7349 | 6.1140 | 0.6251 | 14678 |
| 2025-26 | min | first game of the absence | lightgbm +spells | 4.7357 | 6.1059 | 0.6261 | 14678 |
| 2025-26 | min | first game of the absence | ridge +attrs+standings | 4.7813 | 6.1780 | 0.6172 | 14678 |
| 2025-26 | min | first game of the absence | ridge +attributes | 4.7814 | 6.1802 | 0.6169 | 14678 |
| 2025-26 | min | first game of the absence | ridge no spells | 4.7817 | 6.1822 | 0.6167 | 14678 |
| 2025-26 | min | first game of the absence | ridge +standings | 4.7861 | 6.1853 | 0.6163 | 14678 |
| 2025-26 | min | first game of the absence | ridge +spells | 4.7866 | 6.1874 | 0.6160 | 14678 |
| 2025-26 | min | first game of the absence | baseline last10 | 5.3922 | 7.1123 | 0.4926 | 14678 |
| 2024-25 | min | starter out | lightgbm +standings | 4.6590 | 6.0192 | 0.6625 | 16058 |
| 2024-25 | min | starter out | lightgbm +attrs+standings | 4.6655 | 6.0231 | 0.6620 | 16058 |
| 2024-25 | min | starter out | lightgbm +attributes | 4.6670 | 6.0232 | 0.6620 | 16058 |
| 2024-25 | min | starter out | lightgbm no spells | 4.6807 | 6.0362 | 0.6606 | 16058 |
| 2024-25 | min | starter out | lightgbm +spells | 4.6872 | 6.0374 | 0.6604 | 16058 |
| 2024-25 | min | starter out | ridge no spells | 4.7095 | 6.0770 | 0.6560 | 16058 |
| 2024-25 | min | starter out | ridge +attributes | 4.7116 | 6.0798 | 0.6556 | 16058 |
| 2024-25 | min | starter out | ridge +attrs+standings | 4.7154 | 6.0832 | 0.6552 | 16058 |
| 2024-25 | min | starter out | ridge +spells | 4.7166 | 6.0859 | 0.6549 | 16058 |
| 2024-25 | min | starter out | ridge +standings | 4.7199 | 6.0888 | 0.6546 | 16058 |
| 2024-25 | min | starter out | baseline last10 | 5.2572 | 6.9237 | 0.5534 | 16058 |
| 2025-26 | min | starter out | lightgbm +standings | 4.6958 | 6.0624 | 0.6413 | 20472 |
| 2025-26 | min | starter out | lightgbm +attrs+standings | 4.7013 | 6.0615 | 0.6414 | 20472 |
| 2025-26 | min | starter out | lightgbm +attributes | 4.7065 | 6.0687 | 0.6405 | 20472 |
| 2025-26 | min | starter out | lightgbm +spells | 4.7100 | 6.0707 | 0.6403 | 20472 |
| 2025-26 | min | starter out | lightgbm no spells | 4.7103 | 6.0758 | 0.6397 | 20472 |
| 2025-26 | min | starter out | ridge +attrs+standings | 4.7447 | 6.1275 | 0.6335 | 20472 |
| 2025-26 | min | starter out | ridge +attributes | 4.7476 | 6.1327 | 0.6329 | 20472 |
| 2025-26 | min | starter out | ridge no spells | 4.7487 | 6.1357 | 0.6325 | 20472 |
| 2025-26 | min | starter out | ridge +standings | 4.7488 | 6.1332 | 0.6328 | 20472 |
| 2025-26 | min | starter out | ridge +spells | 4.7515 | 6.1379 | 0.6323 | 20472 |
| 2025-26 | min | starter out | baseline last10 | 5.3114 | 6.9771 | 0.5248 | 20472 |
| 2024-25 | min | starter out, player is a backup | lightgbm +standings | 5.0481 | 6.4118 | 0.4798 | 8731 |
| 2024-25 | min | starter out, player is a backup | lightgbm +attrs+standings | 5.0632 | 6.4194 | 0.4785 | 8731 |
| 2024-25 | min | starter out, player is a backup | lightgbm +attributes | 5.0641 | 6.4237 | 0.4778 | 8731 |
| 2024-25 | min | starter out, player is a backup | lightgbm no spells | 5.0698 | 6.4235 | 0.4779 | 8731 |
| 2024-25 | min | starter out, player is a backup | lightgbm +spells | 5.0823 | 6.4305 | 0.4767 | 8731 |
| 2024-25 | min | starter out, player is a backup | ridge +attrs+standings | 5.1153 | 6.4473 | 0.4740 | 8731 |
| 2024-25 | min | starter out, player is a backup | ridge no spells | 5.1155 | 6.4523 | 0.4732 | 8731 |
| 2024-25 | min | starter out, player is a backup | ridge +attributes | 5.1191 | 6.4496 | 0.4736 | 8731 |
| 2024-25 | min | starter out, player is a backup | ridge +standings | 5.1211 | 6.4548 | 0.4728 | 8731 |
| 2024-25 | min | starter out, player is a backup | ridge +spells | 5.1254 | 6.4570 | 0.4724 | 8731 |
| 2024-25 | min | starter out, player is a backup | baseline last10 | 5.8566 | 7.5817 | 0.2726 | 8731 |
| 2025-26 | min | starter out, player is a backup | lightgbm +standings | 5.0938 | 6.4402 | 0.4664 | 11346 |
| 2025-26 | min | starter out, player is a backup | lightgbm +attrs+standings | 5.1032 | 6.4419 | 0.4661 | 11346 |
| 2025-26 | min | starter out, player is a backup | lightgbm no spells | 5.1061 | 6.4434 | 0.4658 | 11346 |
| 2025-26 | min | starter out, player is a backup | lightgbm +spells | 5.1098 | 6.4445 | 0.4657 | 11346 |
| 2025-26 | min | starter out, player is a backup | lightgbm +attributes | 5.1127 | 6.4525 | 0.4643 | 11346 |
| 2025-26 | min | starter out, player is a backup | ridge +attrs+standings | 5.1331 | 6.4825 | 0.4593 | 11346 |
| 2025-26 | min | starter out, player is a backup | ridge +standings | 5.1350 | 6.4877 | 0.4585 | 11346 |
| 2025-26 | min | starter out, player is a backup | ridge no spells | 5.1393 | 6.4939 | 0.4574 | 11346 |
| 2025-26 | min | starter out, player is a backup | ridge +attributes | 5.1401 | 6.4896 | 0.4581 | 11346 |
| 2025-26 | min | starter out, player is a backup | ridge +spells | 5.1413 | 6.4939 | 0.4574 | 11346 |
| 2025-26 | min | starter out, player is a backup | baseline last10 | 5.9011 | 7.6319 | 0.2506 | 11346 |
| 2024-25 | pts | first game of the absence | ridge pipeline +attributes | 4.4785 | 5.8369 | 0.5481 | 12049 |
| 2024-25 | pts | first game of the absence | ridge pipeline no spells | 4.4787 | 5.8398 | 0.5477 | 12049 |
| 2024-25 | pts | first game of the absence | ridge pipeline +attrs+standings | 4.4799 | 5.8395 | 0.5477 | 12049 |
| 2024-25 | pts | first game of the absence | ridge pipeline +spells | 4.4799 | 5.8398 | 0.5477 | 12049 |
| 2024-25 | pts | first game of the absence | ridge pipeline +standings | 4.4811 | 5.8424 | 0.5473 | 12049 |
| 2024-25 | pts | first game of the absence | lightgbm pipeline +attrs+standings | 4.4875 | 5.8472 | 0.5465 | 12049 |
| 2024-25 | pts | first game of the absence | lightgbm pipeline +standings | 4.4887 | 5.8486 | 0.5463 | 12049 |
| 2024-25 | pts | first game of the absence | lightgbm pipeline no spells | 4.4894 | 5.8514 | 0.5459 | 12049 |
| 2024-25 | pts | first game of the absence | lightgbm pipeline +spells | 4.4911 | 5.8536 | 0.5455 | 12049 |
| 2024-25 | pts | first game of the absence | lightgbm pipeline +attributes | 4.4952 | 5.8602 | 0.5445 | 12049 |
| 2025-26 | pts | first game of the absence | lightgbm pipeline +standings | 4.5084 | 5.9081 | 0.5264 | 14678 |
| 2025-26 | pts | first game of the absence | lightgbm pipeline +attrs+standings | 4.5115 | 5.9049 | 0.5269 | 14678 |
| 2025-26 | pts | first game of the absence | lightgbm pipeline no spells | 4.5174 | 5.9087 | 0.5263 | 14678 |
| 2025-26 | pts | first game of the absence | lightgbm pipeline +attributes | 4.5189 | 5.9117 | 0.5258 | 14678 |
| 2025-26 | pts | first game of the absence | ridge pipeline +standings | 4.5207 | 5.9132 | 0.5256 | 14678 |
| 2025-26 | pts | first game of the absence | ridge pipeline +attrs+standings | 4.5230 | 5.9162 | 0.5251 | 14678 |
| 2025-26 | pts | first game of the absence | ridge pipeline +spells | 4.5233 | 5.9141 | 0.5254 | 14678 |
| 2025-26 | pts | first game of the absence | ridge pipeline no spells | 4.5233 | 5.9131 | 0.5256 | 14678 |
| 2025-26 | pts | first game of the absence | lightgbm pipeline +spells | 4.5247 | 5.9235 | 0.5239 | 14678 |
| 2025-26 | pts | first game of the absence | ridge pipeline +attributes | 4.5259 | 5.9175 | 0.5249 | 14678 |
| 2024-25 | pts | starter out | ridge pipeline no spells | 4.5123 | 5.8713 | 0.5437 | 16058 |
| 2024-25 | pts | starter out | ridge pipeline +spells | 4.5127 | 5.8710 | 0.5437 | 16058 |
| 2024-25 | pts | starter out | ridge pipeline +attributes | 4.5132 | 5.8701 | 0.5439 | 16058 |
| 2024-25 | pts | starter out | ridge pipeline +standings | 4.5144 | 5.8745 | 0.5432 | 16058 |
| 2024-25 | pts | starter out | ridge pipeline +attrs+standings | 4.5154 | 5.8738 | 0.5433 | 16058 |
| 2024-25 | pts | starter out | lightgbm pipeline +attrs+standings | 4.5181 | 5.8955 | 0.5399 | 16058 |
| 2024-25 | pts | starter out | lightgbm pipeline +standings | 4.5194 | 5.8990 | 0.5393 | 16058 |
| 2024-25 | pts | starter out | lightgbm pipeline no spells | 4.5217 | 5.8972 | 0.5396 | 16058 |
| 2024-25 | pts | starter out | lightgbm pipeline +spells | 4.5239 | 5.9031 | 0.5387 | 16058 |
| 2024-25 | pts | starter out | lightgbm pipeline +attributes | 4.5305 | 5.9126 | 0.5372 | 16058 |
| 2025-26 | pts | starter out | lightgbm pipeline +standings | 4.4913 | 5.8770 | 0.5368 | 20472 |
| 2025-26 | pts | starter out | lightgbm pipeline +attrs+standings | 4.4950 | 5.8761 | 0.5369 | 20472 |
| 2025-26 | pts | starter out | lightgbm pipeline no spells | 4.5010 | 5.8803 | 0.5363 | 20472 |
| 2025-26 | pts | starter out | lightgbm pipeline +attributes | 4.5049 | 5.8844 | 0.5356 | 20472 |
| 2025-26 | pts | starter out | lightgbm pipeline +spells | 4.5087 | 5.8950 | 0.5339 | 20472 |
| 2025-26 | pts | starter out | ridge pipeline +standings | 4.5152 | 5.8946 | 0.5340 | 20472 |
| 2025-26 | pts | starter out | ridge pipeline +attrs+standings | 4.5176 | 5.8964 | 0.5337 | 20472 |
| 2025-26 | pts | starter out | ridge pipeline +spells | 4.5195 | 5.8966 | 0.5337 | 20472 |
| 2025-26 | pts | starter out | ridge pipeline no spells | 4.5210 | 5.8961 | 0.5338 | 20472 |
| 2025-26 | pts | starter out | ridge pipeline +attributes | 4.5222 | 5.8989 | 0.5333 | 20472 |
| 2024-25 | pts | starter out, player is a backup | lightgbm pipeline +standings | 3.7800 | 4.9368 | 0.2765 | 8731 |
| 2024-25 | pts | starter out, player is a backup | lightgbm pipeline +attrs+standings | 3.7809 | 4.9390 | 0.2759 | 8731 |
| 2024-25 | pts | starter out, player is a backup | lightgbm pipeline +spells | 3.7827 | 4.9347 | 0.2771 | 8731 |
| 2024-25 | pts | starter out, player is a backup | lightgbm pipeline no spells | 3.7834 | 4.9355 | 0.2769 | 8731 |
| 2024-25 | pts | starter out, player is a backup | ridge pipeline no spells | 3.7834 | 4.9076 | 0.2851 | 8731 |
| 2024-25 | pts | starter out, player is a backup | ridge pipeline +standings | 3.7842 | 4.9087 | 0.2847 | 8731 |
| 2024-25 | pts | starter out, player is a backup | ridge pipeline +spells | 3.7850 | 4.9075 | 0.2851 | 8731 |
| 2024-25 | pts | starter out, player is a backup | ridge pipeline +attrs+standings | 3.7868 | 4.9086 | 0.2848 | 8731 |
| 2024-25 | pts | starter out, player is a backup | ridge pipeline +attributes | 3.7869 | 4.9073 | 0.2852 | 8731 |
| 2024-25 | pts | starter out, player is a backup | lightgbm pipeline +attributes | 3.7909 | 4.9413 | 0.2752 | 8731 |
| 2025-26 | pts | starter out, player is a backup | lightgbm pipeline +standings | 3.8191 | 4.9629 | 0.2950 | 11346 |
| 2025-26 | pts | starter out, player is a backup | lightgbm pipeline +attrs+standings | 3.8277 | 4.9667 | 0.2939 | 11346 |
| 2025-26 | pts | starter out, player is a backup | lightgbm pipeline no spells | 3.8311 | 4.9667 | 0.2939 | 11346 |
| 2025-26 | pts | starter out, player is a backup | lightgbm pipeline +spells | 3.8396 | 4.9700 | 0.2930 | 11346 |
| 2025-26 | pts | starter out, player is a backup | lightgbm pipeline +attributes | 3.8415 | 4.9758 | 0.2913 | 11346 |
| 2025-26 | pts | starter out, player is a backup | ridge pipeline +standings | 3.8497 | 4.9653 | 0.2943 | 11346 |
| 2025-26 | pts | starter out, player is a backup | ridge pipeline +attrs+standings | 3.8506 | 4.9677 | 0.2936 | 11346 |
| 2025-26 | pts | starter out, player is a backup | ridge pipeline +spells | 3.8564 | 4.9681 | 0.2935 | 11346 |
| 2025-26 | pts | starter out, player is a backup | ridge pipeline +attributes | 3.8580 | 4.9713 | 0.2926 | 11346 |
| 2025-26 | pts | starter out, player is a backup | ridge pipeline no spells | 3.8585 | 4.9684 | 0.2934 | 11346 |

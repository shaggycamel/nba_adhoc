SELECT * 
FROM nba.injuries AS inj
LEFT JOIN util.key_dates AS kd 
  ON inj.game_date BETWEEN kd.begin_date AND kd.end_date
WHERE kd.season_type = 'Regular Season'
  AND inj.status IN ('Out', 'Doubtful')
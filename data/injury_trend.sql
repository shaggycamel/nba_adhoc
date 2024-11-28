SELECT 
  dates.season, 
  dates.begin_date AS season_begin_date,
  dates.end_date AS season_end_date,
  tl.date AS announcement_date,
  tl.acc_req,
  tl.team,
  tl.player,
  tl.notes
FROM nba.transaction_log AS tl
INNER JOIN (
	SELECT * 
	FROM util.key_dates
	WHERE season_type = 'Regular Season'
) AS dates ON tl.date BETWEEN dates.begin_date AND dates.end_date
WHERE transaction_type ILIKE 'injur%'

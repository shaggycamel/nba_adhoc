SELECT DISTINCT
  key_dates.season,
  transactions.date, 
  transactions.transaction_type, 
  transactions.team, 
  transactions.player, 
  transactions.acc_req

FROM nba.transaction_log AS transactions
LEFT JOIN util.key_dates AS key_dates 
	ON transactions.date BETWEEN key_dates.begin_date AND key_dates.end_date

WHERE key_dates.season >= '2023-24'
  AND (
    --transactions.transaction_type = 'InjuredList'
    transactions.transaction_type ILIKE 'injur%'
    OR (
      transactions.transaction_type = 'Disciplinary' 
      AND transactions.notes ILIKE '%suspen%'
    )
  )
	

ORDER BY date, player

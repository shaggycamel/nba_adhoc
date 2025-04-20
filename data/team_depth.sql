SELECT 
	schedule.season_type, 
	schedule.game_date, 
	schedule.game_id,
	box_score.team_id,
	team_roster.team_slug, 
	box_score.player_id,
	team_roster.player, 
	team_roster.position, 
	team_roster.height_cm, 
	team_roster.weight_kg, 
	team_roster.birth_date,
	box_score.start_position,
	box_score.comment,
	box_score.min,
	box_score.fgm,
	box_score.fga,
	box_score.fg3_m,
	box_score.fg3_a,
	box_score.ftm,
	box_score.fta,
	box_score.pts,
	box_score.oreb,
	box_score.dreb,
	box_score.reb,
	box_score.ast,
	box_score.stl,
	box_score.blk,
	box_score.tov,
	box_score.pf,
	box_score.plus_minus,
	box_score.e_off_rating,
	box_score.off_rating,
	box_score.e_def_rating,
	box_score.def_rating,
	box_score.e_net_rating,
	box_score.net_rating,
	box_score.usg_pct,
	box_score.e_usg_pct,
	box_score.e_pace,
	box_score.pace,
	box_score.pace_per40,
	box_score.poss,
	box_score.pie
FROM nba.league_game_schedule AS schedule
LEFT JOIN nba.player_box_score AS box_score ON schedule.game_id = box_score.game_id
LEFT JOIN (SELECT * FROM nba.team_roster WHERE season = '2024-25') AS team_roster
	ON box_score.team_id = team_roster.team_id
	AND box_score.player_id = team_roster.player_id

WHERE schedule.season = '2024-25'
  AND box_score.team_id IS NOT NULL
  AND box_score.player_id IS NOT NULL
	AND schedule.game_date <= CURRENT_DATE - 1
	
ORDER BY game_id, team_id, player_id

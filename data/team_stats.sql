SELECT 
	schedule.season,
	schedule.season_type,
	schedule.game_id,
	schedule.game_date,
	box_score.team_id,
	box_score.team_abbreviation AS team_slug,
	box_score.min,
	box_score.plus_minus,
	box_score.off_rating,
	box_score.def_rating,
	box_score.ast_tov,
	box_score.ast_ratio,
	box_score.reb_pct,
	box_score.tm_tov_pct,
	box_score.e_usg_pct,
	box_score.pace,
	box_score.poss,
	box_score.pie
FROM (
	SELECT season_type, season, game_id, game_date, LEFT(matchup, 3) AS team_slug FROM nba.league_game_schedule
	UNION
	SELECT season_type, season, game_id, game_date, RIGHT(matchup, 3) AS team_slug FROM nba.league_game_schedule
) AS schedule
LEFT JOIN nba.team_box_score AS box_score ON schedule.game_id = box_score.game_id
WHERE schedule.season = '2024-25'
  

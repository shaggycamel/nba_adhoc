SELECT 
	schedule.season,
	schedule.season_type,
	roster.team_id,
	roster.team_slug,
	roster.player_id,
	roster.player,
	schedule.game_id,
	schedule.game_date,
	box_score.min
FROM nba.team_roster AS roster
INNER JOIN (
	SELECT season_type, season, game_id, game_date, LEFT(matchup, 3) AS team_slug FROM nba.league_game_schedule
	UNION
	SELECT season_type, season, game_id, game_date, RIGHT(matchup, 3) AS team_slug FROM nba.league_game_schedule
) AS schedule ON roster.season = schedule.season
	AND roster.team_slug = schedule.team_slug
LEFT JOIN nba.player_box_score AS box_score
	ON schedule.game_id = box_score.game_id
	AND roster.team_id = box_score.team_id
	AND roster.player_id = box_score.player_id	
WHERE roster.season = '2024-25'
  AND season_type = 'Regular Season'
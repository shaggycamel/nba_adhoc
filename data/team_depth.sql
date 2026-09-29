
SELECT 
	box_score.season,
	box_score.team_slug,
	box_score.game_date,
	box_score.game_id,
	box_score.player_id,
	box_score.player_name,
	cluster.cluster,
	box_score.min,
	box_score.pts,
	box_score.fga,
	box_score.fgm,
	box_score.fg3_m,
	box_score.fg3_a,
	box_score.ftm,
	box_score.fta,
	box_score.oreb,
	box_score.dreb,
	box_score.reb,
	box_score.ast,
	box_score.stl,
	box_score.blk,
	box_score.tov,
	team_box_score.min AS team_min,
	team_box_score.pts AS team_pts,
	team_box_score.fga AS team_fga,
	team_box_score.fgm AS team_fgm,
	team_box_score.fg3_m AS team_fg3_m,
	team_box_score.fg3_a AS team_fg3_a,
	team_box_score.ftm AS team_ftm,
	team_box_score.fta AS team_fta,
	team_box_score.oreb AS team_oreb,
	team_box_score.dreb AS team_dreb,
	team_box_score.reb AS team_reb,
	team_box_score.ast AS team_ast,
	team_box_score.stl AS team_stl,
	team_box_score.blk AS team_blk,
	team_box_score.tov AS team_tov,
	schedule.AGAINST AS opponent,
	injury.status AS injury_status
	
FROM nba.NBA_PLAYER_BOX_SCORE_VW AS box_score

LEFT JOIN nba.NBA_TEAM_BOX_SCORE_VW AS team_box_score
	ON box_score.SEASON = team_box_score.SEASON
	AND box_score.TEAM_SLUG_BASE = team_box_score.TEAM_ABBREVIATION
	AND box_score.GAME_ID = team_box_score.GAME_ID
	
LEFT JOIN nba.NBA_SCHEDULE_VW AS schedule 
	ON box_score.TEAM_SLUG_BASE = schedule.TEAM	
	AND box_score.GAME_ID = schedule.GAME_ID
	
LEFT JOIN nba.NBA_INJURIES_VW AS injury
	ON box_score.TEAM_SLUG_BASE = injury.TEAM_SLUG
	AND box_score.player_id = injury.NBA_ID
	AND box_score.GAME_ID = injury.GAME_ID
	
LEFT JOIN anl.player_cluster AS cluster
	ON box_score.season = cluster.SEASON
	AND box_score.PLAYER_ID = cluster.PLAYER_ID

WHERE box_score.season >= '2020-21'
	AND box_score.SEASON_TYPE = 'Regular Season'
	
ORDER BY box_score.game_id, box_score.team_slug, box_score.player_id

	

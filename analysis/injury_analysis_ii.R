
library(tidyverse)

# Raw ---------------------------------------------------------------------

db_con <- nba.dataRub::dh_createCon("cockroach")

df_inj <- nba.dataRub::dh_getQuery(db_con, "injury_trend_ii.sql") |> 
  inner_join(
    nba.dataRub::dh_getQuery(db_con, "SELECT season, team_abbreviation, player_id, min FROM nba.player_season_stats") |>
      mutate(player_id = as.integer(player_id)) |> 
      bind_rows(
        nba.dataRub::dh_getQuery(db_con, "
          SELECT lgs.season, bs.team_abbreviation, bs.player_id, SUM(bs.min) AS min
          FROM nba.player_box_score AS bs
          INNER JOIN nba.league_game_schedule AS lgs ON bs.game_id = lgs.game_id
          	AND lgs.season = '2024-25'
          	AND lgs.season_type = 'Regular Season'
          GROUP BY lgs.season, bs.team_abbreviation, bs.player_id
         ")  
      ) |>
      slice_max(min, n = 7, by = c(season, team_abbreviation)) |> 
      distinct(season, player_id) |> 
      mutate(player_id = as.double(player_id)),
    join_by(season, nba_id == player_id)
  ) |> 
  mutate(
    season_progress = 
      as.numeric(difftime(game_date, begin_date, units = "days")) /
        as.numeric(difftime(end_date, begin_date, units = "days")),
    season_progress_bin = cut(season_progress, breaks = 10, labels = FALSE) / 10
  )


# Histogram ---------------------------------------------------------------

ggplot(df_inj, aes(x = season_progress)) +
  geom_histogram(fill = "dodgerblue", colour = "black") +
  scale_x_continuous(labels = scales::label_percent()) +
  theme_minimal() +
  labs(
    title = "Starting Players face more injuries in last quarter of Season",
    subtitle = "Seasons 2021-22 to 2024-25",
    x = "Progress through Season",
    y = "Starting player injured count"
  )


# Trend -------------------------------------------------------------------

df_inj |> 
  filter(!(season == "2024-25" & season_progress_bin == 1)) |> 
  summarise(count = n(), .by = c(season, season_progress_bin)) |> 
  ggplot(aes(x = season_progress_bin, y = count, colour = season)) +
  stat_smooth(level = 0.3) +
  scale_x_continuous(labels = scales::label_percent(), breaks = seq(0, 1, 0.1)) +
  theme_bw() +
  labs(
    title = "Starting Players face more injuries in last quarter of  Regular Season",
    x = "Progress through Regular Season",
    y = "Starting player injured count"
  )


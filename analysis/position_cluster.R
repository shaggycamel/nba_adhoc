
library(nba.dataRub)
library(tidyverse)
library(cluster)

db_con <- dh_createCon("postgres")

df <- dh_getQuery(db_con, read_file(here::here("data", "position_cluster.sql"))) |> 
  filter(!is.na(game_date)) |> 
  mutate(age = difftime(game_date, birthdate, units = "weeks") / 52)
  
cols <- list("non-model-cols" = c("season_type", "season", "player_id", "player_slug", "birthdate", "position", "game_id", "game_date"))
cols[["model-cols"]] <- setdiff(colnames(df), cols[["non-model-cols"]])

df <- mutate(df, across(all_of(cols[["model-cols"]]), \(x) replace_na(x, 0)))

clus <- select(df, all_of(cols[["model-cols"]])) |> 
  clara(k = 7)

df["cluster"] <- clus$clustering

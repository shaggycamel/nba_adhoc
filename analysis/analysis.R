
library(tidyverse)



# Raw ---------------------------------------------------------------------

db_con <- nba.dataRub::dh_createCon("cockroach")
df_raw <- nba.dataRub::dh_getQuery(db_con, "injury_trend.sql")
df_season_dates <- distinct(select(df_raw, starts_with("season")))


# Prep --------------------------------------------------------------------

df <- df_raw |> 
  arrange(player, announcement_date) |> 
  mutate(
    running_tally = c(TRUE, acc_req[-1L] != acc_req[-length(acc_req)]),
    .by = c(player, season)
  ) |> 
  filter(running_tally) |> 
  mutate(injure_date = if_else(acc_req == "Relinquished", announcement_date, NA_Date_)) |> 
  group_by(player, season) |> 
  fill(injure_date) |> 
  ungroup() |> 
  select(-notes) |> 
  pivot_wider(names_from = acc_req, values_from = announcement_date) |> 
  na.omit()


df_plt <- map(1:nrow(df), \(x){
  df[x, ] |> 
    pivot_longer(c(Relinquished, Acquired), names_to = "acc_req", values_to = "span") |> 
    complete(span = seq.Date(min(span), max(span), by="day")) |> 
    select(-ends_with("date"), -running_tally) |> 
    fill(season, team, player) |> 
    mutate(acc_req = replace_na(acc_req, "Injured")) |> 
    select(season, team, player, acc_req, span)
}) |> list_rbind() |> 
  left_join(df_season_dates, by = join_by(season, between(span, season_begin_date, season_end_date))) |> 
  mutate(week = as.integer((span - season_begin_date) / 7) + 1) |> 
  filter(week <= 10)



# Plot --------------------------------------------------------------------
# Maybe group by week and calucate variability across seasons
# Then plot latest season over the variabnce

# To do this, need to bring in season dates and calc number of
# days since start of season to injury

df_p2 <- df_raw |> 
  mutate(week = as.integer((announcement_date - season_begin_date) / 7) + 1) |> 
  filter(week <= 10) |> 
  count(season, week)



anl_season <- "2024-25"
df_p <- count(df_plt, season, week) |> filter(season > "2017-18")

ggplot(
  filter(df_p2, season == anl_season),
  aes(x = week, y = n)
) +
  geom_path(aes(colour = season), linewidth = 1) +
  geom_point(show.legend = TRUE) +
  stat_smooth(
    data = filter(df_p2, season < anl_season),
    mapping = aes(x = week, y = n, colour = "2013-24 avg"),
  ) + 
  ylim(0, NA) +
  scale_x_continuous(breaks = 1:max(df_p$week)) +
  scale_colour_brewer(type = "qual", palette = "Set1", direction = -1) +
  theme_bw() +
  labs(
    title = "NBA Injury Announcement Count", 
    subtitle = "2013-2024 Average vs 2024-25",
    x = "Week", 
    y = "Count"
  )
   
  

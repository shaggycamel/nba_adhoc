
library(nba.dataRub)
library(tidyverse)
library(ClusterR)
library(recipes)

db_con <- dh_createCon("postgres")

# Variables ------------------------------------------------------------------

df_raw <- dh_getQuery(db_con, read_file(here::here("data", "position_cluster.sql"))) |> 
  filter(!is.na(game_date))
  
cols <- list("non-model-cols" = c("season_type", "season", "player_id", "player_slug", "birthdate", "position", "game_id", "game_date"))
cols[["model-cols"]] <- setdiff(colnames(df_raw), cols[["non-model-cols"]])

df_clus <- df_raw |> 
  filter(season_type %in% c("Regular Season", "Playoffs")) |> 
  mutate(
    age = difftime(game_date, birthdate, units = "weeks") / 52,
    across(all_of(cols[["model-cols"]]), \(x) replace_na(x, 0))
  ) |> 
  summarise(
    across(c(height_cm, weight_kg), \(x) max(x)),
    gp = n(),
    across(
      c(min, fgm, fga, fg3_m, fg3_a, ftm, fta, pts, oreb, dreb, ast, stl, blk, tov, pf),
      \(x) sum(x),
      .names = "{.col}_sum"
    ),
    .by = str_subset(cols$`non-model-cols`, "game|position|type", negate = TRUE)
  ) |> 
  mutate(
    age_yrs = as.integer(difftime(as.Date(paste0(str_sub(season, end = 4), "-01-01")), birthdate, units = "weeks") / 52),
    across(ends_with("_sum"), \(x) x / gp, .names = "{.col}_mean")
  ) |> 
  rename_with(~ str_replace(.x, "sum_mean", "mean"), , .cols = contains("sum_mean"))

# Data ------------------------------------------------------------------

df_train <- filter(df_clus, between(season, "2019-20", "2023-24")) 
df_test <- filter(df_clus, season == "2024-25") 

rec <- recipe(df_train) |> 
  step_scale(ends_with("_mean")) |> 
  # step_pca(ends_with("_mean"), num_comp = 6) |> 
  prep()

# Cluster ------------------------------------------------------------------

df_train_prep <- select(bake(rec, new_data = NULL), height_cm, weight_kg, ends_with("mean"))

mod_cluster <- Clara_Medoids(
  data = df_train_prep,
  clusters = 8,
  samples = 5,
  sample_size = 0.2,
  fuzzy = TRUE,
  seed = 123
)

mod_cluster$fuzzy_probs |> View()

df_train_prep["cluster"] <- ordered(mod_cluster$clusters)
df_train["cluster"] <- ordered(mod_cluster$clusters)

# Plots ------------------------------------------------------------------

# One dimension
df_train_prep |> 
  select(weight_kg, height_cm, cluster, ends_with("mean")) |> 
  pivot_longer(-cluster) |> 
  nest_by(name, .keep = TRUE) |> 
  mutate(plt = list(
    ggplot(data, aes(x = value, y = cluster)) +
      ggridges::stat_density_ridges() +
      labs(title = name)
  )) |> 
  pull(plt)

# Two dimensions
prepped_cols <- colnames(df_train_prep)
as.tibble(t(combn(1:(ncol(df_train_prep)-1), 2))) |> 
  pwalk(\(...){
    ls <- list(...)
    x_dim <- sym(prepped_cols[ls[["V1"]]])
    y_dim <- sym(prepped_cols[ls[["V2"]]])

    plot(
      df_train_prep |> 
        ggplot(aes(x = !!x_dim, y = !!y_dim, colour = cluster)) + 
        geom_point()
    )

  })

# Sense?
select(df_train, season, player_slug, cluster) |> 
  View(">")

# Predictions ------------------------------------------------------------------

df_test <- df_test |> 
  mutate(cluster = predict(
    mod_cluster, 
    select(bake(rec, new_data = df_test), height_cm, weight_kg, ends_with("mean"))
  ))

df_test |> 
  select(cluster, player_slug) |> 
  View(">")


# Ingest position clustering into DB
# Save models to file and predict on data every quarter

library(tidyverse)
library(tidymodels)
library(nba.dataRub)
library(magrittr)
library(here)
library(slider)


# Data --------------------------------------------------------------------

db_con <- dh_createCon("cockroach")
# df_team_depth <- dh_getQuery(db_con, read_file(here("data", "team_depth.sql")))


# Injured players ---------------------------------------------------------

df_team_stats <- dh_getQuery(db_con, read_file(here("data", "team_stats.sql"))) |> 
  na.omit() |> 
  distinct() |>  # fix this in sql
  mutate(across(matches("min|plus|rating|ast_tov|pct|^p"), \(x) lag(x, order_by = game_date)), .by = team_id)

# All xmas day games missing for some reason....
df_player_min <- dh_getQuery(db_con, read_file(here("data", "player_min.sql"))) |> 
  mutate(min = replace_na(min, 0)) |> 
  left_join(
    select(df_team_stats, -c(starts_with("season"), game_date, team_slug)) |> 
      rename_with(\(x) paste0("opp_", x)),
    by = join_by(game_id == opp_game_id),
    relationship = "many-to-many"
  ) |> 
  filter(team_id != opp_team_id)


model_date <- as.Date("2024-12-23")
df_train <- filter(df_player_min, game_date < model_date)
df_test <- filter(df_player_min, game_date == model_date)


df_team_player_top_mins <- df_train |> 
  summarise(min = sum(min), .by = c(team_id, team_slug, player_id, player)) |> 
  slice_max(min, prop = 0.75, by = team_id)


df_slide_mins <- df_train |> 
  arrange(game_date) |>
  mutate(min = slide_dbl(min, ~ median(.x, na.rm = TRUE), .before = 3, .after = -1), .by = player_id)


df_minutes_mod <- df_train |> 
  nest_by(team_id, team_slug, .keep = TRUE) |> 
  mutate(data = list(
    data |> 
      cross_join(
        select(df_slide_mins, ends_with("_id"), player, min) |> 
          rename_with(\(x) paste0("ind_", x))      
      ) |> 
      filter(
        ind_team_id == team_id,
        ind_game_id == game_id, 
        (ind_player_id %in% df_team_player_top_mins$player_id | player_id == ind_player_id)
      ) |> 
      filter(sum(min) > 10, .by = player_id) |>
      nest_by(player_id, player) |> 
      mutate(data = list(pivot_wider(
        data,
        id_cols = c(game_id, min, starts_with("opp")),
        names_from = ind_player,
        values_from = ind_min
      ))) |> 
      mutate(mod_lm = list(tryCatch(MASS::stepAIC(lm(min ~ ., data = select(data, -ends_with("_id")), na.action = na.omit), trace = FALSE), error = \(e) NA))) |>
      mutate(mod_dt = list(rpart::rpart(min ~ ., data = select(data, -ends_with("_id")), model = TRUE))) |>
      mutate(mod_rf = list(fit(rand_forest(mode="regression", engine="randomForest"), min ~ ., data = select(data, -ends_with("_id")))))
  )) |> 
  unnest(data) |>
  ungroup() |>
  # filter(!is.na(mod_lm)) |>
  select(-data)


# Linear Regression plot
df_minutes_mod |> 
  filter(!is.na(mod_lm)) %$% 
  map2(player, mod_lm, \(pl, mod){
    tidy(mod, conf.int = TRUE) |>
      filter(term != "(Intercept)", !is.na(term)) |>
      mutate(term = str_remove_all(term, "`")) |> 
      ggplot(aes(estimate, reorder(term, estimate), xmin = conf.low, xmax = conf.high, height = 0)) +
      geom_point() +
      geom_vline(xintercept = 0, lty = 4) +
      geom_errorbarh() +
      theme_bw() +
      labs(title = pl, y = NULL)
  })

# Decision Tree plot
df_minutes_mod |> 
  filter(team_slug == "BOS") %$% 
  map2(player, mod_dt, \(pl, mod){
    rpart.plot::rpart.plot(mod, main = pl)
  })

# Random Forest VIP plot
df_minutes_mod |> 
  filter(team_slug == "BOS") %$% 
  map2(player, mod_rf, \(pl, mod){
    extract_fit_engine(mod) |> 
      randomForest::varImpPlot(main = pl)
  })



inj_players <- filter(df_test, min == 0)$player_id


# INTEGRATE INJURY MINS (0) INTO DATA
df_minutes_pred <- df_test |> 
  select(matches("^team|^player|^game|^opp"), actual_min = min) |> 
  cross_join(
    df_slide_mins |> 
      slice_max(game_date, by = team_slug) |>
      mutate(min = if_else(player_id %in% inj_players, 0, min)) |>
      select(team_slug, player, min, game_date) |> 
      rename_with(\(x) paste0("ind_", x)),
  ) |> 
  filter(team_slug == ind_team_slug) |> 
  left_join(select(df_minutes_mod, player_id, starts_with("mod"))) |> 
  nest_by(team_id, team_slug, player_id, player, game_id, actual_min, mod_lm, mod_dt, mod_rf) |> 
  mutate(data = list(pivot_wider(data, names_from = ind_player, values_from = ind_min))) |> 
  filter(!is.null(mod_dt)) |>
  mutate(
  #   pred_lm = case_when(
  #     player_id %in% inj_players ~ 0,
  #     is.null(mod_lm) ~ NA,
  #     .default = predict(mod_lm, data)
  #   ),
    pred_dt = case_when(
      player_id %in% inj_players ~ 0,
      is.null(mod_dt) ~ NA,
      .default = predict(mod_dt, data)
    ),
    pred_rf = case_when(
      player_id %in% inj_players ~ 0,
      is.null(mod_rf) ~ NA,
      .default = predict(mod_rf, data)[[1]]
    )
  ) |> 
  mutate(
    # diff_lm = actual_min - pred_lm, 
    diff_dt = actual_min - pred_dt,
    diff_rf = actual_min - pred_rf,
    top_min_player = player_id %in% df_team_player_top_mins$player_id
  ) |> 
  ungroup() 

df_minutes_pred |> 
  select(team_slug, player, top_min_player, matches("min|pred|diff")) |> 
  view("check")


ggplot(df_minutes_pred, aes(x = diff_dt)) +
  geom_density() +
  facet_grid(rows = vars(top_min_player))

hist(df_minutes_pred$diff_dt)


# JOIN INJURED PLAYERS ONTO LINEUP ----------------------------------------

df_injuries <- dh_getQuery(db_con, read_file(here("data", "injuries.sql"))) |> 
  mutate(acc_req = str_to_lower(acc_req)) 

# NEED TO DEAL WITH "Injury" records

  filter(!(acc_req == "acquired" & lag(acc_req) == "acquired"), .by = c(season, player, transaction_type, team)) |>
  mutate(appearance = row_number() / 2, .by = c(season, player, transaction_type, team)) |>
  mutate(appearance = if_else(appearance %% 1 == 0.5, appearance + 0.5, appearance)) |>
  pivot_wider(id_cols = c(season, transaction_type, team, player), names_from = acc_req, values_from = date) |> 
  mutate(relinquished = keep_at(pluck(relinquished, 1, .default = NA), 1), .by = everything()) |>
  mutate(acquired = keep_at(pluck(acquired, 1, .default = NA), 1), .by = everything()) |> 
  mutate(
    player = str_remove(player, "\\(.*\\)"),
    player = str_remove(player, ".* /"),
    player = str_remove_all(player, "[:punct:]"),
    player = str_replace_all(player, c("Herb"="Herbert")), # Add other names here
    player = str_squish(player)
  ) |> 
  left_join(
    dh_getQuery(db_con, "SELECT team_name, team_slug FROM nba.teams"),
    join_by(team == team_name)
  ) |> 
  fuzzyjoin::stringdist_left_join(
    dh_getQuery(db_con, "SELECT nba_name, nba_id, espn_id, yahoo_id FROM util.nba_fty_name_match"),
    by = join_by(player == nba_name),
    max_dist = 3,
    distance_col = "dist",
    ignore_case = TRUE
  ) |> 
  slice_min(dist, with_ties = FALSE, by = c(player, relinquished)) |> 
  select(-c(player, dist)) |> 
  filter(!is.na(nba_name))


# Injured on game log -----------------------------------------------------

df |> 
  left_join(
    df_injuries,
    by = join_by(team_slug, game_date >= relinquished, game_date <= acquired)
  ) |> View("<")


# COME BACK TO THIS -------------------------------------------------------



df_top_mins <- df |> 
  mutate(across(where(is.numeric), \(x) replace_na(x, 0))) |> 
  mutate(top_mins = min_rank(desc(min)) <= 7, .by = c(game_id, team_id)) |> 
  mutate(across(all_of(colnames(df)[14:length(colnames(df))]), \(x) lag(x, order_by = game_date)), .by = player_id)

df_split <- initial_split(df_top_mins, strata = top_mins)
df_test <- testing(df_split)
df_train <- training(df_split)

# Recipe ------------------------------------------------------------------

rec_original <- recipe(
    data = df_train, 
    top_mins ~ min + pf + plus_minus + e_off_rating + e_def_rating + e_net_rating + e_usg_pct + e_pace + poss + pie,
  ) |> 
  # update_role(player_id, game_id, game_date, new_role = "id") |> 
  step_naomit()
  # step_log(c(all_numeric_predictors(), -c(weight_kg, height_cm, avg_min, avg_plus_minus, ends_with("pct"))), offset = 1) |> 
  # step_range(all_numeric_predictors()) |> 
  # step_pca(all_numeric_predictors()) |> 
  # step_dummy(all_factor_predictors())
  
recs <- mget(str_subset(objects(), "^rec_"))

# x <- bake(prep(rec_original), new_data = NULL)
# map(recs, \(x) count(bake(prep(x), new_data = NULL), over_20_pts))


# Model Definitions -------------------------------------------------------

bag_tree_C5.0_spec <- bag_tree() |>
  set_engine("C5.0") |>
  set_mode("classification")

boost_tree_C5.0_spec <- boost_tree(trees = tune(), min_n = tune(), sample_size = tune()) |>
  set_engine("C5.0")

decision_tree_C5.0_spec <- decision_tree(min_n = tune()) |>
  set_engine("C5.0")

logistic_reg_glmnet_spec <- logistic_reg(penalty = tune(), mixture = tune()) |>
  set_engine("glmnet")

rand_forest_ranger_spec <- rand_forest(mtry = tune(), min_n = tune()) |>
  set_engine("ranger") |>
  set_mode("classification")

svm_poly_kernlab_spec <- svm_poly(cost = tune(), degree = tune(), scale_factor = tune(), margin = tune()) |>
  set_engine("kernlab") |>
  set_mode("classification")

mods <- mget(str_subset(objects(), "_spec$"))
mods <- lst(rand_forest_ranger_spec)

# Workflow ----------------------------------------------------------------

# Register parallel backend
registerDoParallel(cores = parallel::detectCores(logical = FALSE) / 2)

# Tune models
cv_folds <- vfold_cv(df_train, v = 5)
wflows <- workflow_set(recs, mods) |>
  workflow_map(
    "tune_grid",
    resamples = cv_folds,
    grid = 10,
    verbose = TRUE,
    metrics = metric_set(roc_auc, accuracy),
    control = control_grid(parallel_over = "everything", verbose = TRUE)
  )


suppressPackageStartupMessages({
  library(magrittr)
  library(dplyr)
  library(readr)
  library(stringr)
  library(tidyr)
  library(ggplot2)
  library(sf)
  library(lme4)
  library(performance)
  library(tibble)
  library(here)
})

if (!exists("%>%")) {
  `%>%` <- magrittr::`%>%`
}

# =============================================================================
# Personality Master Analysis
# One-script pipeline for:
# 1) Edge speed/distance metrics + merge into combined_edges
# 2) Behavioral cluster scores and change summaries
# 3) PCA analyses
# 4) GLMM analyses (raw outcomes + cluster outcomes)
# 5) Repeated-profile consistency check across season/task contexts
# =============================================================================

# -----------------------------------------------------------------------------
# CONFIG
# -----------------------------------------------------------------------------
paths <- list(
  cue_tracking = here("data", "raw", "dataverse_files", "cue", "processed", "master_results.csv"),
  cue_doors = here("data", "raw", "cue", "trial_door.csv"),
  cue_coords = here("data", "raw", "cue", "coords.csv"),
  foraging_edges = here("data", "raw", "dataverse_files", "foraging", "assets", "foraging_edges.csv"),
  combined_edges = here("data", "processed", "combined_edges.csv"),
  latency = here("data", "raw", "latency.csv"),
  maze = here("data", "raw", "dataverse_files", "maze", "processed", "maze_results.csv")
)

outputs <- list(
  edge_speed_distance = here("data", "processed", "edge_speed_distance_by_trial.csv"),
  cluster_scores = here("data", "processed", "behaviour_cluster_scores_by_trial.csv"),
  t1_vs_later = here("data", "processed", "behaviour_change_t1_vs_later.csv"),
  seasonal_change = here("data", "processed", "behaviour_change_by_season.csv"),
  pca_edge_scores = here("data", "processed", "pca_edge_scores_by_trial.csv"),
  pca_edge_loadings = here("data", "processed", "pca_edge_loadings.csv"),
  pca_maze_scores = here("data", "processed", "pca_maze_scores_by_trial.csv"),
  pca_maze_loadings = here("data", "processed", "pca_maze_loadings.csv"),
  pca_individual_scores = here("data", "processed", "pca_individual_season_scores.csv"),
  pca_individual_loadings = here("data", "processed", "pca_individual_season_loadings.csv"),
  glmm_raw_summary = here("data", "processed", "glmm_raw_hypothesis_table.csv"),
  glmm_raw_coefficients = here("data", "processed", "glmm_raw_best_model_coefficients.csv"),
  glmm_cluster_summary = here("data", "processed", "cluster_glmm_hypothesis_table.csv"),
  glmm_cluster_coefficients = here("data", "processed", "cluster_glmm_best_model_coefficients.csv"),
  repeated_profile_consistency = here("data", "processed", "repeated_profile_consistency.csv"),
  master_hypothesis_table = here("data", "processed", "hypothesis_decision_table.csv"),
  plot_trait_by_task = here("output", "figures", "personality_master", "cluster_traits_by_task.png"),
  plot_trait_by_season = here("output", "figures", "personality_master", "cluster_traits_by_season.png"),
  plot_repeated_heatmap = here("output", "figures", "personality_master", "repeated_id_profile_heatmap.png")
)

dir.create(here("output", "figures", "personality_master"), recursive = TRUE, showWarnings = FALSE)

foraging_cm_per_pixel <- 0.187192
season_order <- c("spring", "summer", "winter")

# -----------------------------------------------------------------------------
# HELPERS
# -----------------------------------------------------------------------------
zscore <- function(x) {
  s <- sd(x, na.rm = TRUE)
  if (is.na(s) || s == 0) return(rep(0, length(x)))
  (x - mean(x, na.rm = TRUE)) / s
}

clean_key <- function(x) {
  x %>%
    as.character() %>%
    str_replace_all("\\s+", "") %>%
    str_replace_all("_+", "_")
}

parse_trial_num <- function(trial_label) {
  trial_label <- as.character(trial_label)
  trial_num <- str_extract(trial_label, "^T\\d+")
  as.numeric(str_remove(trial_num, "T"))
}

calculate_distance <- function(df) {
  if (nrow(df) < 2) return(0)
  df <- df %>%
    arrange(frame) %>%
    mutate(
      x_lag = lag(x),
      y_lag = lag(y),
      dist = sqrt((x - x_lag)^2 + (y - y_lag)^2)
    )
  sum(df$dist, na.rm = TRUE)
}

calculate_intersection <- function(x1, y1, x2, y2, x3, y3, x4, y4) {
  denom <- (y4 - y3) * (x2 - x1) - (x4 - x3) * (y2 - y1)
  if (denom == 0) return(c(NA_real_, NA_real_))
  ua <- ((x4 - x3) * (y1 - y3) - (y4 - y3) * (x1 - x3)) / denom
  x <- x1 + ua * (x2 - x1)
  y <- y1 + ua * (y2 - y1)
  c(x, y)
}

safe_icc <- function(model) {
  vc <- as.data.frame(VarCorr(model))
  id_var <- vc$vcov[vc$grp == "ID"]
  res_var <- sigma(model)^2
  if (length(id_var) == 0 || is.na(id_var) || is.na(res_var) || (id_var + res_var) == 0) return(NA_real_)
  as.numeric(id_var / (id_var + res_var))
}

fit_glmm_family <- function(data, response_var) {
  dat <- data %>%
    select(ID, season, task, trial_num, all_of(response_var)) %>%
    rename(response = all_of(response_var)) %>%
    filter(!is.na(response), !is.na(ID), !is.na(season), !is.na(task)) %>%
    mutate(
      ID = factor(ID),
      season = factor(season),
      task = factor(task)
    )

  if (nrow(dat) < 30 || n_distinct(dat$ID) < 5) {
    return(list(
      summary = tibble(
        response = response_var,
        n_obs = nrow(dat),
        n_id = n_distinct(dat$ID),
        best_model = NA_character_,
        ICC_ID = NA_real_,
        H0_no_personality = "insufficient_data",
        H1_personality = "insufficient_data",
        H2_season_stronger = "insufficient_data"
      ),
      coefs = tibble()
    ))
  }

  m_null <- lmer(response ~ 1 + (1 | ID), data = dat, REML = FALSE)

  has_season <- n_distinct(dat$season) > 1
  has_task <- n_distinct(dat$task) > 1
  has_trial <- all(!is.na(dat$trial_num))

  if (has_season && has_task && has_trial) {
    m_context <- lmer(response ~ season + task + trial_num + (1 | ID), data = dat, REML = FALSE)
  } else if (has_season && has_task) {
    m_context <- lmer(response ~ season + task + (1 | ID), data = dat, REML = FALSE)
  } else if (has_season && has_trial) {
    m_context <- lmer(response ~ season + trial_num + (1 | ID), data = dat, REML = FALSE)
  } else if (has_season) {
    m_context <- lmer(response ~ season + (1 | ID), data = dat, REML = FALSE)
  } else if (has_task && has_trial) {
    m_context <- lmer(response ~ task + trial_num + (1 | ID), data = dat, REML = FALSE)
  } else if (has_task) {
    m_context <- lmer(response ~ task + (1 | ID), data = dat, REML = FALSE)
  } else if (has_trial) {
    m_context <- lmer(response ~ trial_num + (1 | ID), data = dat, REML = FALSE)
  } else {
    m_context <- m_null
  }

  best_model <- ifelse(AIC(m_context) < AIC(m_null), "context_model", "m_null")
  best_fit <- if (best_model == "context_model") m_context else m_null

  icc_id <- safe_icc(m_null)

  summary_tbl <- tibble(
    response = response_var,
    n_obs = nrow(dat),
    n_id = n_distinct(dat$ID),
    best_model = best_model,
    ICC_ID = icc_id,
    H0_no_personality = ifelse(!is.na(icc_id) && icc_id <= 0.2, "not_rejected", "rejected"),
    H1_personality = ifelse(!is.na(icc_id) && icc_id > 0.2, "supported", "weak_or_not_supported"),
    H2_season_stronger = ifelse(best_model == "context_model", "possible", "weak")
  )

  coef_tbl <- as.data.frame(coef(summary(best_fit))) %>%
    rownames_to_column("term") %>%
    mutate(response = response_var, best_model = best_model) %>%
    select(response, best_model, term, Estimate, `Std. Error`, `t value`)

  list(summary = summary_tbl, coefs = coef_tbl)
}

# -----------------------------------------------------------------------------
# 1) EDGE SPEED/DISTANCE + MERGE INTO COMBINED_EDGES
# -----------------------------------------------------------------------------
cat("[1/5] Computing edge speed/distance metrics...\n")

cue_tracking <- read_csv(paths$cue_tracking, show_col_types = FALSE)
cue_doors <- read_csv(paths$cue_doors, show_col_types = FALSE) %>% mutate(trial = paste0("T", trial_n))
cue_coords <- read_csv(paths$cue_coords, show_col_types = FALSE)

cue_trials <- split(cue_tracking, cue_tracking$unique_trial_ID)

process_cue_trial <- function(trial_data) {
  season_filter <- if (any(grepl("winter_2024", trial_data$unique_trial_ID))) "winter_2024" else "other"

  trial_door_id <- cue_doors %>%
    filter(trial == unique(trial_data$trial), season == season_filter) %>%
    pull(door)

  if (length(trial_door_id) == 0) return(NULL)

  coords_trial <- cue_coords %>%
    filter(unique_trial_ID == unique(trial_data$unique_trial_ID)) %>%
    select(4:11) %>%
    pivot_longer(cols = contains("x"), names_to = "door", values_to = "x") %>%
    bind_cols(
      cue_coords %>%
        filter(unique_trial_ID == unique(trial_data$unique_trial_ID)) %>%
        select(4:11) %>%
        pivot_longer(cols = contains("y"), names_to = "door", values_to = "y") %>%
        mutate(door_ID = substr(door, 1, 1)) %>%
        select(door_ID, y)
    )

  if (nrow(coords_trial) < 4) return(NULL)

  intersection <- calculate_intersection(
    coords_trial$x[1], coords_trial$y[1], coords_trial$x[3], coords_trial$y[3],
    coords_trial$x[2], coords_trial$y[2], coords_trial$x[4], coords_trial$y[4]
  )
  if (any(is.na(intersection))) return(NULL)

  side_lines <- st_multilinestring(list(
    rbind(c(coords_trial$x[1], coords_trial$y[1]), c(coords_trial$x[2], coords_trial$y[2])),
    rbind(c(coords_trial$x[2], coords_trial$y[2]), c(coords_trial$x[3], coords_trial$y[3])),
    rbind(c(coords_trial$x[3], coords_trial$y[3]), c(coords_trial$x[4], coords_trial$y[4])),
    rbind(c(coords_trial$x[4], coords_trial$y[4]), c(coords_trial$x[1], coords_trial$y[1]))
  )) %>% st_sfc() %>% st_sf()

  edges_buffer <- st_buffer(side_lines, dist = 4)
  track_sf <- trial_data %>% st_as_sf(coords = c("x", "y"))

  trial_data <- trial_data %>%
    mutate(at_edge = as.vector(st_intersects(track_sf, edges_buffer, sparse = FALSE))) %>%
    mutate(out_edge = !at_edge)

  total_distance_at_edge <- calculate_distance(trial_data %>% filter(at_edge))
  total_distance_out_edge <- calculate_distance(trial_data %>% filter(out_edge))

  time_at_edge <- sum(trial_data$time[trial_data$at_edge], na.rm = TRUE)
  time_out_edge <- sum(trial_data$time[trial_data$out_edge], na.rm = TRUE)

  tibble(
    task = "cue",
    unique_trial_ID = as.character(trial_data$unique_trial_ID[1]),
    season = as.character(trial_data$season[1]),
    ID = as.character(trial_data$ID[1]),
    trial = as.character(trial_data$trial[1]),
    mean_speed_at_edge_cm_s = ifelse(time_at_edge > 0, total_distance_at_edge / (time_at_edge / 1000), NA_real_),
    mean_speed_out_edge_cm_s = ifelse(time_out_edge > 0, total_distance_out_edge / (time_out_edge / 1000), NA_real_),
    total_distance_at_edge_cm = total_distance_at_edge,
    total_distance_out_edge_cm = total_distance_out_edge
  )
}

cue_metrics <- bind_rows(lapply(cue_trials, process_cue_trial))

foraging_edges <- read_csv(paths$foraging_edges, show_col_types = FALSE)
foraging_metrics <- foraging_edges %>%
  transmute(
    task = as.character(task),
    unique_trial_ID = as.character(unique_trial_ID),
    season = as.character(season),
    ID = as.character(ID),
    trial = as.character(trial),
    mean_speed_at_edge_cm_s = ifelse(
      time_edge > 0,
      (path_length_edge * foraging_cm_per_pixel) / (time_edge / 1000),
      NA_real_
    ),
    mean_speed_out_edge_cm_s = ifelse(
      time_center > 0,
      (path_length_center * foraging_cm_per_pixel) / (time_center / 1000),
      NA_real_
    ),
    total_distance_at_edge_cm = path_length_edge * foraging_cm_per_pixel,
    total_distance_out_edge_cm = path_length_center * foraging_cm_per_pixel
  )

edge_speed_distance <- bind_rows(cue_metrics, foraging_metrics) %>%
  arrange(task, ID, season, trial)

write_csv(edge_speed_distance, outputs$edge_speed_distance)

combined_edges <- read_csv(paths$combined_edges, show_col_types = FALSE) %>%
  select(-any_of(c(
    "mean_speed_at_edge_cm_s", "mean_speed_out_edge_cm_s",
    "total_distance_at_edge_cm", "total_distance_out_edge_cm"
  )))

combined_edges <- combined_edges %>%
  left_join(
    edge_speed_distance %>%
      select(unique_trial_ID, mean_speed_at_edge_cm_s, mean_speed_out_edge_cm_s,
             total_distance_at_edge_cm, total_distance_out_edge_cm),
    by = "unique_trial_ID"
  )

write_csv(combined_edges, paths$combined_edges)
cat("[1/5] Done.\n")

# -----------------------------------------------------------------------------
# 2) CLUSTER SCORES + CHANGE SUMMARIES
# -----------------------------------------------------------------------------
cat("[2/5] Building behavioral cluster scores...\n")

edges <- read_csv(paths$combined_edges, show_col_types = FALSE) %>%
  mutate(
    unique_trial_ID = clean_key(unique_trial_ID),
    ID = as.character(ID),
    season = str_to_lower(str_trim(season)),
    task = str_to_lower(str_trim(task)),
    trial_num = parse_trial_num(trial)
  )

latency <- read_csv(paths$latency, show_col_types = FALSE) %>%
  rename(unique_trial_ID = unique_trial_id, ID = id) %>%
  mutate(
    task = if ("task" %in% names(.)) task else test
  ) %>%
  mutate(
    unique_trial_ID = clean_key(unique_trial_ID),
    ID = as.character(ID),
    season = str_to_lower(str_trim(season)),
    task = str_to_lower(str_trim(task)),
    trial_num = parse_trial_num(trial)
  )

maze <- read_csv(paths$maze, show_col_types = FALSE) %>%
  mutate(
    unique_trial_ID = clean_key(unique_trial_ID),
    ID = as.character(ID),
    season = str_to_lower(str_trim(season)),
    trial_num = parse_trial_num(trial)
  )

latency_task <- latency

# Build one unified trial table so cue/foraging, od, and maze are all represented as tasks.
edge_base <- edges %>%
  transmute(
    unique_trial_ID, ID, season, trial, trial_num, task,
    time_at_edge, percent_time_at_edge, total_distance_out_edge_cm,
    mean_speed, mean_speed_at_edge_cm_s, mean_speed_out_edge_cm_s,
    total_trial_time = as.numeric(total_trial_time),
    emergence = NA_real_,
    total_deviations = NA_real_,
    total_deviation_length = NA_real_
  )

latency_base <- latency_task %>%
  transmute(
    unique_trial_ID, ID, season, trial, trial_num,
    task,
    time_at_edge = NA_real_,
    percent_time_at_edge = NA_real_,
    total_distance_out_edge_cm = NA_real_,
    mean_speed = NA_real_,
    mean_speed_at_edge_cm_s = NA_real_,
    mean_speed_out_edge_cm_s = NA_real_,
    total_trial_time = as.numeric(emergence),
    emergence = as.numeric(emergence),
    total_deviations = NA_real_,
    total_deviation_length = NA_real_
  )

maze_base <- maze %>%
  transmute(
    unique_trial_ID, ID, season, trial, trial_num,
    task = "maze",
    time_at_edge = NA_real_,
    percent_time_at_edge = NA_real_,
    total_distance_out_edge_cm = NA_real_,
    mean_speed = NA_real_,
    mean_speed_at_edge_cm_s = NA_real_,
    mean_speed_out_edge_cm_s = NA_real_,
    total_trial_time = NA_real_,
    emergence = NA_real_,
    total_deviations = as.numeric(total_deviations),
    total_deviation_length = as.numeric(total_deviation_length)
  )

all_trials <- bind_rows(edge_base, latency_base, maze_base) %>%
  distinct(unique_trial_ID, task, .keep_all = TRUE)

trial_scores <- all_trials %>%
  mutate(
    z_time_at_edge_low = -zscore(time_at_edge),
    z_percent_edge_low = -zscore(percent_time_at_edge),
    z_out_dist_high = zscore(total_distance_out_edge_cm),
    z_mean_speed_total = zscore(mean_speed),
    z_total_trial_time = zscore(total_trial_time),
    edge_speed_bias = ifelse(
      (mean_speed_out_edge_cm_s + mean_speed_at_edge_cm_s) > 0,
      (mean_speed_out_edge_cm_s - mean_speed_at_edge_cm_s) /
        (mean_speed_out_edge_cm_s + mean_speed_at_edge_cm_s),
      NA_real_
    ),
    z_edge_speed_bias = zscore(edge_speed_bias),
    z_latency_low = -zscore(emergence),
    z_dev_num_high = zscore(total_deviations),
    z_dev_cm_high = zscore(total_deviation_length)
  ) %>%
  mutate(
    boldness_cluster = rowMeans(
      cbind(z_time_at_edge_low, z_percent_edge_low, z_out_dist_high,
            z_dev_num_high, z_dev_cm_high, z_latency_low),
      na.rm = TRUE
    ),
    activity_cluster = rowMeans(cbind(z_mean_speed_total, z_total_trial_time), na.rm = TRUE),
    boldness_edge_speed_trait = z_edge_speed_bias,

    exploration_base = rowMeans(
      cbind(z_out_dist_high, z_dev_num_high, z_dev_cm_high, z_latency_low),
      na.rm = TRUE
    ),
    exploration_cluster = ifelse(trial_num == 1, exploration_base * 1.5, exploration_base),

    boldness_cluster_strict = if_else(
      !is.na(z_time_at_edge_low) & !is.na(z_percent_edge_low) & !is.na(z_out_dist_high) &
        !is.na(z_dev_num_high) & !is.na(z_dev_cm_high) & !is.na(z_latency_low),
      rowMeans(cbind(z_time_at_edge_low, z_percent_edge_low, z_out_dist_high,
                     z_dev_num_high, z_dev_cm_high, z_latency_low)),
      NA_real_
    )
  )

write_csv(trial_scores, outputs$cluster_scores)

t1_vs_later <- trial_scores %>%
  mutate(is_t1 = trial_num == 1) %>%
  group_by(ID, season, task) %>%
  summarise(
    n_trials = n(),
    boldness_t1 = mean(boldness_cluster[is_t1], na.rm = TRUE),
    boldness_later = mean(boldness_cluster[!is_t1], na.rm = TRUE),
    boldness_change_t1_to_later = boldness_later - boldness_t1,
    exploration_t1 = mean(exploration_cluster[is_t1], na.rm = TRUE),
    exploration_later = mean(exploration_cluster[!is_t1], na.rm = TRUE),
    exploration_change_t1_to_later = exploration_later - exploration_t1,
    activity_t1 = mean(activity_cluster[is_t1], na.rm = TRUE),
    activity_later = mean(activity_cluster[!is_t1], na.rm = TRUE),
    activity_change_t1_to_later = activity_later - activity_t1,
    edge_speed_trait_t1 = mean(boldness_edge_speed_trait[is_t1], na.rm = TRUE),
    edge_speed_trait_later = mean(boldness_edge_speed_trait[!is_t1], na.rm = TRUE),
    edge_speed_trait_change_t1_to_later = edge_speed_trait_later - edge_speed_trait_t1,
    .groups = "drop"
  ) %>%
  mutate(across(contains("_to_later"), ~ ifelse(is.nan(.x), NA_real_, .x)))

write_csv(t1_vs_later, outputs$t1_vs_later)

id_season <- trial_scores %>%
  group_by(ID, season) %>%
  summarise(
    boldness_mean = mean(boldness_cluster, na.rm = TRUE),
    exploration_mean = mean(exploration_cluster, na.rm = TRUE),
    activity_mean = mean(activity_cluster, na.rm = TRUE),
    edge_speed_trait_mean = mean(boldness_edge_speed_trait, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(season = factor(season, levels = season_order, ordered = TRUE)) %>%
  arrange(ID, season) %>%
  group_by(ID) %>%
  mutate(
    boldness_change_from_prev = boldness_mean - lag(boldness_mean),
    exploration_change_from_prev = exploration_mean - lag(exploration_mean),
    activity_change_from_prev = activity_mean - lag(activity_mean),
    edge_speed_trait_change_from_prev = edge_speed_trait_mean - lag(edge_speed_trait_mean)
  ) %>%
  ungroup()

write_csv(id_season, outputs$seasonal_change)
cat("[2/5] Done.\n")

# -----------------------------------------------------------------------------
# 3) PCA (EDGE, MAZE, INDIVIDUAL x SEASON)
# -----------------------------------------------------------------------------
cat("[3/5] Running PCA analyses...\n")

edge_pca_vars <- c(
  "percent_time_at_edge", "mean_speed_at_edge_cm_s", "mean_speed_out_edge_cm_s",
  "total_distance_at_edge_cm", "total_distance_out_edge_cm"
)

edges_complete <- edges %>%
  select(unique_trial_ID, ID, season, task, all_of(edge_pca_vars)) %>%
  drop_na()

edge_pca <- prcomp(edges_complete[, edge_pca_vars], scale. = TRUE, center = TRUE)
edge_scores <- edges_complete %>%
  bind_cols(as.data.frame(edge_pca$x) %>% select(PC1, PC2, PC3))
edge_loadings <- as.data.frame(edge_pca$rotation) %>%
  rownames_to_column("variable")

write_csv(edge_scores, outputs$pca_edge_scores)
write_csv(edge_loadings, outputs$pca_edge_loadings)

maze_complete <- maze %>%
  select(unique_trial_ID, ID, season, trial, total_deviations, total_deviation_length) %>%
  drop_na()

maze_pca <- prcomp(maze_complete[, c("total_deviations", "total_deviation_length")], scale. = TRUE, center = TRUE)
maze_scores <- maze_complete %>%
  bind_cols(as.data.frame(maze_pca$x) %>% rename(maze_PC1 = PC1, maze_PC2 = PC2))
maze_loadings <- as.data.frame(maze_pca$rotation) %>%
  rownames_to_column("variable")

write_csv(maze_scores, outputs$pca_maze_scores)
write_csv(maze_loadings, outputs$pca_maze_loadings)

edges_by_ind <- edges %>%
  group_by(ID, season) %>%
  summarise(across(all_of(edge_pca_vars), ~ mean(.x, na.rm = TRUE)), .groups = "drop")

latency_by_ind <- latency_task %>%
  group_by(ID, season) %>%
  summarise(mean_emergence = mean(emergence, na.rm = TRUE), .groups = "drop")

maze_by_ind <- maze %>%
  group_by(ID, season) %>%
  summarise(
    mean_total_deviations = mean(total_deviations, na.rm = TRUE),
    mean_total_deviation_length = mean(total_deviation_length, na.rm = TRUE),
    .groups = "drop"
  )

ind_season <- edges_by_ind %>%
  full_join(latency_by_ind, by = c("ID", "season")) %>%
  full_join(maze_by_ind, by = c("ID", "season"))

ind_vars <- c(edge_pca_vars, "mean_emergence", "mean_total_deviations", "mean_total_deviation_length")
ind_complete <- ind_season %>% drop_na(all_of(ind_vars))

if (nrow(ind_complete) >= 5) {
  ind_pca <- prcomp(ind_complete[, ind_vars], scale. = TRUE, center = TRUE)
  ind_scores <- ind_complete %>%
    bind_cols(as.data.frame(ind_pca$x) %>% rename(ind_PC1 = PC1, ind_PC2 = PC2, ind_PC3 = PC3))
  ind_loadings <- as.data.frame(ind_pca$rotation) %>% rownames_to_column("variable")
  write_csv(ind_scores, outputs$pca_individual_scores)
  write_csv(ind_loadings, outputs$pca_individual_loadings)
} else {
  write_csv(ind_complete, outputs$pca_individual_scores)
  write_csv(tibble(variable = character(), ind_PC1 = numeric()), outputs$pca_individual_loadings)
}

cat("[3/5] Done.\n")

# -----------------------------------------------------------------------------
# 4) GLMMS (RAW RESPONSES + CLUSTER RESPONSES)
# -----------------------------------------------------------------------------
cat("[4/5] Running GLMM analyses...\n")

raw_data <- edges %>%
  left_join(latency_task %>% select(unique_trial_ID, emergence) %>% distinct(), by = "unique_trial_ID") %>%
  left_join(maze %>% select(unique_trial_ID, total_deviation_length), by = "unique_trial_ID") %>%
  left_join(edge_scores %>% select(unique_trial_ID, PC1), by = "unique_trial_ID")

raw_responses <- c("percent_time_at_edge", "emergence", "total_deviation_length", "PC1")
raw_results <- lapply(raw_responses, function(r) fit_glmm_family(raw_data, r))
raw_summary <- bind_rows(lapply(raw_results, function(x) x$summary))
raw_coefs <- bind_rows(lapply(raw_results, function(x) x$coefs))

write_csv(raw_summary, outputs$glmm_raw_summary)
write_csv(raw_coefs, outputs$glmm_raw_coefficients)

cluster_responses <- c("boldness_cluster", "exploration_cluster", "activity_cluster", "boldness_edge_speed_trait")
cluster_results <- lapply(cluster_responses, function(r) fit_glmm_family(trial_scores, r))
cluster_summary <- bind_rows(lapply(cluster_results, function(x) x$summary))
cluster_coefs <- bind_rows(lapply(cluster_results, function(x) x$coefs))

write_csv(cluster_summary, outputs$glmm_cluster_summary)
write_csv(cluster_coefs, outputs$glmm_cluster_coefficients)

master_hypothesis <- bind_rows(
  raw_summary %>% mutate(group = "raw"),
  cluster_summary %>% mutate(group = "cluster")
) %>%
  select(group, everything())

write_csv(master_hypothesis, outputs$master_hypothesis_table)
cat("[4/5] Done.\n")

# -----------------------------------------------------------------------------
# 5) REPEATED PROFILE CONSISTENCY CHECK
# -----------------------------------------------------------------------------
cat("[5/6] Checking repeated-profile consistency...\n")

profile_tbl <- trial_scores %>%
  group_by(ID, season, task) %>%
  summarise(
    boldness = mean(boldness_cluster, na.rm = TRUE),
    exploration = mean(exploration_cluster, na.rm = TRUE),
    activity = mean(activity_cluster, na.rm = TRUE),
    edge_trait = mean(boldness_edge_speed_trait, na.rm = TRUE),
    n_trials = n(),
    .groups = "drop"
  ) %>%
  filter(!if_any(c(boldness, exploration, activity, edge_trait), is.nan))

id_summary <- profile_tbl %>%
  group_by(ID) %>%
  summarise(
    n_profiles = n(),
    n_seasons = n_distinct(season),
    n_tasks = n_distinct(task),
    repeated = n_profiles > 1 & (n_seasons > 1 | n_tasks > 1),
    .groups = "drop"
  )

rep_ids <- id_summary %>% filter(repeated) %>% pull(ID)
profile_rep <- profile_tbl %>% filter(ID %in% rep_ids)

within_tbl <- profile_rep %>%
  group_by(ID) %>%
  group_modify(~ {
    d <- dist(as.matrix(select(.x, boldness, exploration, activity, edge_trait)))
    tibble(mean_within_dist = ifelse(length(d) > 0, mean(as.numeric(d)), NA_real_), n_pairs = length(d))
  }) %>%
  ungroup()

between_tbl <- profile_rep %>%
  select(ID, season, task, boldness, exploration, activity, edge_trait) %>%
  inner_join(
    profile_rep %>% select(ID2 = ID, season, task, boldness2 = boldness,
                           exploration2 = exploration, activity2 = activity, edge_trait2 = edge_trait),
    by = c("season", "task"),
    relationship = "many-to-many"
  ) %>%
  filter(ID < ID2) %>%
  mutate(dist = sqrt(
    (boldness - boldness2)^2 +
      (exploration - exploration2)^2 +
      (activity - activity2)^2 +
      (edge_trait - edge_trait2)^2
  ))

between_mean <- mean(between_tbl$dist, na.rm = TRUE)

consistency_tbl <- within_tbl %>%
  left_join(id_summary, by = "ID") %>%
  mutate(
    between_mean_matched_context = between_mean,
    profile_consistent = mean_within_dist < between_mean,
    margin = between_mean - mean_within_dist
  ) %>%
  arrange(mean_within_dist)

write_csv(consistency_tbl, outputs$repeated_profile_consistency)
cat("[5/6] Done.\n\n")

# -----------------------------------------------------------------------------
# 6) PLOTS
# -----------------------------------------------------------------------------
cat("[6/6] Creating summary plots...\n")

trait_long <- trial_scores %>%
  select(ID, season, task, boldness_cluster, exploration_cluster, activity_cluster, boldness_edge_speed_trait) %>%
  pivot_longer(
    cols = c(boldness_cluster, exploration_cluster, activity_cluster, boldness_edge_speed_trait),
    names_to = "trait",
    values_to = "score"
  ) %>%
  filter(!is.na(score)) %>%
  mutate(
    trait = factor(trait, levels = c("boldness_cluster", "exploration_cluster", "activity_cluster", "boldness_edge_speed_trait")),
    season = factor(season, levels = season_order, ordered = TRUE)
  )

p_task <- ggplot(trait_long, aes(x = task, y = score, fill = task)) +
  geom_boxplot(outlier.alpha = 0.2) +
  facet_wrap(~ trait, scales = "free_y") +
  labs(
    title = "Cluster Trait Distributions by Task",
    x = "Task",
    y = "Trait Score"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")

ggsave(outputs$plot_trait_by_task, p_task, width = 11, height = 7, dpi = 300)

p_season <- ggplot(trait_long, aes(x = season, y = score, fill = season)) +
  geom_boxplot(outlier.alpha = 0.2) +
  facet_wrap(~ trait, scales = "free_y") +
  labs(
    title = "Cluster Trait Distributions by Season",
    x = "Season",
    y = "Trait Score"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")

ggsave(outputs$plot_trait_by_season, p_season, width = 11, height = 7, dpi = 300)

heat_df <- profile_tbl %>%
  filter(ID %in% rep_ids) %>%
  mutate(context = paste(season, task, sep = " | ")) %>%
  select(ID, context, boldness, exploration, activity, edge_trait) %>%
  pivot_longer(
    cols = c(boldness, exploration, activity, edge_trait),
    names_to = "trait",
    values_to = "score"
  )

p_heat <- ggplot(heat_df, aes(x = context, y = ID, fill = score)) +
  geom_tile() +
  facet_wrap(~ trait, scales = "free_x") +
  scale_fill_gradient2(low = "#2166ac", mid = "#f7f7f7", high = "#b2182b", midpoint = 0) +
  labs(
    title = "Repeated-ID Personality Profiles Across Season and Task",
    x = "Season | Task",
    y = "ID",
    fill = "Score"
  ) +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(outputs$plot_repeated_heatmap, p_heat, width = 13, height = 8, dpi = 300)

cat("[6/6] Done.\n\n")

# -----------------------------------------------------------------------------
# FINAL CONSOLE SUMMARY
# -----------------------------------------------------------------------------
cat("=== Pipeline Complete ===\n")
cat("Saved files:\n")
print(unlist(outputs))

cat("\nKey cluster GLMM ICCs:\n")
print(cluster_summary %>% select(response, n_obs, n_id, best_model, ICC_ID, H1_personality))

cat("\nRepeated-profile consistency:\n")
cat("Repeated IDs:", length(rep_ids), "\n")
cat("Consistent IDs (within < between):", sum(consistency_tbl$profile_consistent, na.rm = TRUE), "/", nrow(consistency_tbl), "\n")

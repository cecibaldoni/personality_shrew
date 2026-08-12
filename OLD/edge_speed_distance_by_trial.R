library(dplyr)
library(readr)
library(sf)
library(tidyr)

# Paths
root_dir <- "/Users/narctaz/Desktop/personality"

cue_tracking_path <- file.path(root_dir, "dataverse_files", "cue", "processed", "master_results.csv")
cue_doors_path <- file.path(root_dir, "cue", "trial_door.csv")
cue_coords_path <- file.path(root_dir, "cue", "coords.csv")

foraging_edges_path <- file.path(root_dir, "dataverse_files", "foraging", "assets", "foraging_edges.csv")

output_path <- file.path(root_dir, "personality", "edge_speed_distance_by_trial.csv")

# Foraging conversion from pixels to cm
foraging_cm_per_pixel <- 0.187192

# Utility: sum step distances in coordinate units
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

# Utility: line intersection for rectangular arena center
calculate_intersection <- function(x1, y1, x2, y2, x3, y3, x4, y4) {
  denom <- (y4 - y3) * (x2 - x1) - (x4 - x3) * (y2 - y1)
  if (denom == 0) return(c(NA_real_, NA_real_))
  ua <- ((x4 - x3) * (y1 - y3) - (y4 - y3) * (x1 - x3)) / denom
  x <- x1 + ua * (x2 - x1)
  y <- y1 + ua * (y2 - y1)
  c(x, y)
}

# Build per-trial metrics for cue using same edge definition as existing scripts
cue_tracking <- read_csv(cue_tracking_path, show_col_types = FALSE)
cue_doors <- read_csv(cue_doors_path, show_col_types = FALSE) %>%
  mutate(trial = paste0("T", trial_n))
cue_coords <- read_csv(cue_coords_path, show_col_types = FALSE)

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

  mean_speed_at_edge <- ifelse(time_at_edge > 0, total_distance_at_edge / (time_at_edge / 1000), NA_real_)
  mean_speed_out_edge <- ifelse(time_out_edge > 0, total_distance_out_edge / (time_out_edge / 1000), NA_real_)

  tibble(
    task = "cue",
    unique_trial_ID = as.character(trial_data$unique_trial_ID[1]),
    season = as.character(trial_data$season[1]),
    ID = as.character(trial_data$ID[1]),
    trial = as.character(trial_data$trial[1]),
    mean_speed_at_edge_cm_s = mean_speed_at_edge,
    mean_speed_out_edge_cm_s = mean_speed_out_edge,
    total_distance_at_edge_cm = total_distance_at_edge,
    total_distance_out_edge_cm = total_distance_out_edge
  )
}

cue_metrics <- lapply(cue_trials, process_cue_trial) %>%
  bind_rows()

# Build per-trial metrics for foraging from existing edge summary
foraging_edges <- read_csv(foraging_edges_path, show_col_types = FALSE)

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

combined_metrics <- bind_rows(cue_metrics, foraging_metrics) %>%
  arrange(task, ID, season, trial)

write_csv(combined_metrics, output_path)

cat("Saved per-trial edge speed and distance metrics:\n", output_path, "\n", sep = "")
cat("Rows written:", nrow(combined_metrics), "\n")

# Merge into combined_edges.csv
combined_edges_path <- file.path(root_dir, "personality", "combined_edges.csv")
combined_edges <- read_csv(combined_edges_path, show_col_types = FALSE)

# Remove old edge speed/distance columns if already present (avoid duplicates on re-run)
combined_edges <- combined_edges %>%
  select(-any_of(c("mean_speed_at_edge_cm_s", "mean_speed_out_edge_cm_s",
                    "total_distance_at_edge_cm", "total_distance_out_edge_cm")))

combined_edges_updated <- combined_edges %>%
  left_join(
    combined_metrics %>% select(unique_trial_ID, mean_speed_at_edge_cm_s,
                                mean_speed_out_edge_cm_s, total_distance_at_edge_cm,
                                total_distance_out_edge_cm),
    by = "unique_trial_ID"
  )

write_csv(combined_edges_updated, combined_edges_path)

cat("Updated combined_edges.csv with edge speed/distance columns:\n", combined_edges_path, "\n", sep = "")
cat("Missing joins:", sum(is.na(combined_edges_updated$mean_speed_at_edge_cm_s)), "\n")

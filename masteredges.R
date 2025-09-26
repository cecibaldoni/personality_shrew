library(dplyr)
library(sf)
library(readr)
library(tidyr)

#----------------------------
# 1. Load Data
#----------------------------
tracking_foodjourney <- read_csv("/Users/narctaz/Desktop/personality/cue/results/master_results.csv") %>% 
  mutate(
    unique_trial_ID = as.factor(unique_trial_ID),
    season = as.factor(season),
    ID = as.factor(ID),
    food_journey = as.factor(food_journey)
  )

#calculate total time per trial and %
trial_total_time <- tracking_foodjourney %>%
  group_by(unique_trial_ID) %>%
  summarize(total_trial_time = sum(time, na.rm = TRUE), .groups = "drop")



trial_ls2 <- split(tracking_foodjourney, tracking_foodjourney$unique_trial_ID)

doors <- read.csv("/Users/narctaz/Desktop/personality/cue/trial_door.csv") %>%
  mutate(trial = paste0("T", trial_n))

coords <- read_csv("/Users/narctaz/Desktop/personality/cue/coords.csv")

edges_file <- "/Users/narctaz/Desktop/personality/cue/results/masteredges.csv"

#----------------------------
# 2. Utility Functions
#----------------------------
calculate_intersection <- function(x1, y1, x2, y2, x3, y3, x4, y4) {
  denom <- (y4 - y3) * (x2 - x1) - (x4 - x3) * (y2 - y1)
  if (denom == 0) return(c(NA, NA))
  ua <- ((x4 - x3) * (y1 - y3) - (y4 - y3) * (x1 - x3)) / denom
  x <- x1 + ua * (x2 - x1)
  y <- y1 + ua * (y2 - y1)
  return(c(x, y))
}

calculate_perpendicular_vector <- function(x1, y1, x2, y2) {
  dx <- x2 - x1
  dy <- y2 - y1
  mag <- sqrt(dx^2 + dy^2)
  if (mag == 0) return(c(0, 0))
  unit_vector <- c(dx / mag, dy / mag)
  return(c(-unit_vector[2], unit_vector[1]))
}

calculate_distance <- function(df) {
  df <- df %>%
    arrange(frame) %>%
    mutate(
      x_lag = lag(x),
      y_lag = lag(y),
      dist = sqrt((x - x_lag)^2 + (y - y_lag)^2)
    )
  sum(df$dist, na.rm = TRUE)
}

#----------------------------
# 3. Function to Process a Single Trial
#----------------------------
process_trial <- function(trial_data) {
  
  season_filter <- if (any(grepl("winter_2024", trial_data$unique_trial_ID))) "winter_2024" else "other"
  track_sf <- trial_data %>% st_as_sf(coords = c("x", "y"))
  
  trial_door_ID <- doors %>%
    filter(trial == unique(trial_data$trial), season == season_filter) %>%
    pull(door)
  
  coords_trial <- coords %>%
    filter(unique_trial_ID == unique(trial_data$unique_trial_ID)) %>%
    select(4:11) %>%
    pivot_longer(cols = contains("x"), names_to = "door", values_to = "x") %>%
    bind_cols(
      coords %>%
        filter(unique_trial_ID == unique(trial_data$unique_trial_ID)) %>%
        select(4:11) %>%
        pivot_longer(cols = contains("y"), names_to = "door", values_to = "y") %>%
        mutate(door_ID = substr(door,1,1)) %>%
        select(door_ID, y)
    )
  
  if(nrow(coords_trial) < 4) return(NULL) # skip trial if not enough points
  
  intersection <- calculate_intersection(
    coords_trial$x[1], coords_trial$y[1], coords_trial$x[3], coords_trial$y[3],
    coords_trial$x[2], coords_trial$y[2], coords_trial$x[4], coords_trial$y[4]
  )
  if(any(is.na(intersection))) return(NULL) # skip if intersection invalid
  
  side_lines <- st_multilinestring(list(
    rbind(c(coords_trial$x[1], coords_trial$y[1]), c(coords_trial$x[2], coords_trial$y[2])),
    rbind(c(coords_trial$x[2], coords_trial$y[2]), c(coords_trial$x[3], coords_trial$y[3])),
    rbind(c(coords_trial$x[3], coords_trial$y[3]), c(coords_trial$x[4], coords_trial$y[4])),
    rbind(c(coords_trial$x[4], coords_trial$y[4]), c(coords_trial$x[1], coords_trial$y[1]))
  )) %>% st_sfc() %>% st_sf()
  
  edges_buffer <- st_buffer(side_lines, dist = 4)
  
  trial_data <- trial_data %>%
    mutate(
      at_edge = as.vector(st_intersects(track_sf, edges_buffer, sparse = FALSE)),
      out_edge = !at_edge
    )
  
  time_at_edge <- trial_data %>%
    filter(at_edge) %>%
    summarize(total_time = sum(time)) %>%
    pull(total_time)
  
  df <- data.frame(
    season = trial_data$season[1],
    ID = trial_data$ID[1],
    trial = trial_data$trial[1],
    time_at_edge = time_at_edge,
    at_edge_cm = calculate_distance(trial_data %>% filter(at_edge)),
    out_edge_cm = calculate_distance(trial_data %>% filter(out_edge)),
    edge_to_cm = calculate_distance(trial_data %>% filter(food_journey == "trip_to" & at_edge)),
    edge_back_cm = calculate_distance(trial_data %>% filter(food_journey == "trip_back" & at_edge)),
    out_edge_to_cm = calculate_distance(trial_data %>% filter(food_journey == "trip_to" & out_edge)),
    out_edge_back_cm = calculate_distance(trial_data %>% filter(food_journey == "trip_back" & out_edge))
  )
  
  return(df)
}


# 4. Run lapply on All Trials

trial_results <- lapply(trial_ls2, process_trial)
trial_results <- do.call(rbind, trial_results) # combine all results


#----------------------------
# 5. Save to CSV
#----------------------------
#calculate total distance back per trial and %
trial_results <- trial_results %>%
  mutate(unique_trial_ID = paste(season, ID, trial, sep = "_")) %>%
  # Join in total trial time
  left_join(trial_total_time, by = "unique_trial_ID") %>%
  # Calculate percent time at edge
  mutate(percent_time_at_edge = (time_at_edge / total_trial_time) * 100) %>%
  select(unique_trial_ID, everything())#reorder columns

write.table(trial_results, file = edges_file, append = TRUE, sep = ",",
            row.names = FALSE, col.names = !file.exists(edges_file))
#6. T1

trial_results_T1 <- trial_results %>% filter(trial == "T1")
trial_results_other <- trial_results %>% filter(trial != "T1")

#same individual
compare_T1_vs_others <- trial_results_other %>%
  left_join(trial_results_T1 %>% select(unique_trial_ID, percent_time_at_edge),
            by = "unique_trial_ID",
            suffix = c("_other", "_T1"))

df_plot <- trial_results %>%
  filter(ID %in% unique(compare_T1_vs_others$ID)) %>%
  mutate(
    trial_type = ifelse(trial == "T1", "T1", "Other"),
    season = as.factor(season)
  )

# Ensure numeric trial ordering: T1 → T10
df_plot <- df_plot %>%
  mutate(trial = factor(trial, levels = paste0("T", 1:10)))
ids <- unique(df_plot$ID)

for (id in ids) {
  df_id <- df_plot %>% filter(ID == id)
  
  p <- ggplot(df_id, aes(x = trial, y = percent_time_at_edge)) +
    # Other trials: colored by season
    geom_point(data = df_id %>% filter(trial_type == "Other"),
               aes(color = season),
               size = 3, position = position_jitter(width = 0.1)) +
    # T1 trial: always red
    geom_point(data = df_id %>% filter(trial_type == "T1"),
               color = "red", shape = 17, size = 3) +
    # Optional: horizontal line for T1
    geom_hline(data = df_id %>% filter(trial_type == "T1"),
               aes(yintercept = percent_time_at_edge),
               color = "red", linetype = "dashed") +
    scale_color_brewer(palette = "Set2") +
    labs(
      title = paste("T1 vs Other Trials for Individual", id),
      x = "Trial",
      y = "Percent Time at Edge (%)",
      color = "Season"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      panel.background = element_rect(fill = "white"),
      plot.background = element_rect(fill = "white")
    )
  
  # Save per individual
  ggsave(
    filename = paste0(output_folder, "T1_comparison_", id, ".png"),
    plot = p,
    width = 6,
    height = 4,
    dpi = 300
  )
}



#across individuals
T1_across_individuals <- trial_results %>% filter(trial == "T1")

ggplot(T1_across_individuals, aes(x = ID, y = percent_time_at_edge, fill = season)) +
  geom_col(position = "dodge") +
  labs(
    title = "T1 Percent Time at Edge Across Individuals",
    x = "Individual ID",
    y = "Percent Time at Edge (%)",
    fill = "season"
  ) +
  theme_minimal()

# Foraging master edges and combination of cue and foraging (time)
# 0. configuration ---------------------------------------------------
library(dplyr)      # data manipulation
library(readr)      # reading csv files
library(ggplot2)    # plotting
library(stringr)
# ------------------------------------------------------------------

# path to the “new” dataset you described
input_file  <- "/Users/narctaz/Desktop/personality/dataverse_files/foraging/assets/foraging_edges.csv"
output_file <- "/Users/narctaz/Desktop/personality/dataverse_files/foraging/assets/fmasteredges.csv"   # optional write‑out

# ------------------------------------------------------------------
# 1. read and inspect ------------------------------------------------
df <- read_csv(input_file,
               col_types = cols(
                 task               = col_character(),
                 unique_trial_ID    = col_factor(),
                 ID                 = col_factor(),
                 trial              = col_factor(),
                 season             = col_factor(),
                 time_total         = col_double(),
                 time_edge          = col_double(),
                 time_center        = col_double(),
                 prop_time_edge     = col_double(),
                 path_length_edge   = col_double(),
                 path_length_center = col_double(),
                 prop_path_length_edge = col_double()
               ))

glimpse(df)
# make sure there are no NAs in the time columns
stopifnot(!any(is.na(df$time_total)))
stopifnot(!any(is.na(df$time_edge)))

# ------------------------------------------------------------------
# 2. calculate supplementary columns -------------------------------
df <- df %>%
  mutate(
    percent_time_at_edge   = (time_edge / time_total) * 100,
  )

# 3. rename columns --------------------------------------------------
df <- df %>%
  rename(
    total_trial_time = time_total,
    time_at_edge     = time_edge
  )
# ------------------------------------------------------------------

# per‑trial comparison with a reference trial (e.g. T1) –
# exactly the same pattern as you used before:

trialT1S1 <- df %>% filter(grepl("^T1S1", trial))
compare_T1S1 <- df %>%
  left_join(trialT1S1 %>% select(unique_trial_ID, percent_time_at_edge),
            by = "unique_trial_ID",
            suffix = c("_other","_T1S1"))

df_plot <- df %>%
  filter(ID %in% unique(compare_T1S1$ID)) %>%
  mutate(
    trial_type = ifelse(grepl("^T1S1", trial), "T1S1", "Other"),
    season = as.factor(season)
  )

# Ensure proper trial ordering
df_plot <- df_plot %>%
  mutate(trial = factor(trial, levels = c("T1S1", "T1S2", "T2S1", "T2S2")))
ids <- unique(df_plot$ID)

for (id in ids) {
  df_id <- df_plot %>% filter(ID == id)
  
  p <- ggplot(df_id, aes(x = trial, y = percent_time_at_edge)) +
    geom_point(data = df_id %>% filter(trial_type == "Other"),
               aes(color = season),
               size = 3, position = position_jitter(width = 0.1)) +
    geom_point(data = df_id %>% filter(trial_type == "T1S1"),
               color = "red", shape = 17, size = 3) +
    geom_hline(data = df_id %>% filter(trial_type == "T1S1"),
               aes(yintercept = percent_time_at_edge),
               color = "red", linetype = "dashed") +
    scale_color_brewer(palette = "Set2") +
    labs(
      title = paste("T1S1 vs Other Trials for Individual", id),
      x = "Trial",
      y = "Percent Time at Edge (%)",
      color = "Season"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      panel.background = element_rect(fill = "white"),
      plot.background = element_rect(fill = "white")
    )
  
  ggsave(
    filename = paste0("/Users/narctaz/Desktop/personality/PNG", "T1S1_comparison_", id, ".png"),
    plot = p,
    width = 6,
    height = 4,
    dpi = 300
  )
}


# 5. optional save --------------------------------------------------
write_csv(df, output_file)

# Combine masteredges.csv and fmasteredges.csv -------------------
# read masteredges.csv
masteredges_path <- "/Users/narctaz/Desktop/personality/cue/results/masteredges.csv"
masteredges <- read_csv(masteredges_path)

# add task column with "cue"
masteredges <- masteredges %>% mutate(task = "cue")

# read fmasteredges.csv
foraging_path <- "/Users/narctaz/Desktop/personality/dataverse_files/foraging/assets/fmasteredges.csv"
fmasteredges <- read_csv(foraging_path)

# find common columns
common_cols <- intersect(names(masteredges), names(fmasteredges))

# select common columns from both
masteredges_common <- masteredges %>% select(all_of(common_cols))
foraging_common <- fmasteredges %>% select(all_of(common_cols))

# combine
combined_df <- bind_rows(masteredges_common, foraging_common)

# save combined
combined_output <- "/Users/narctaz/Desktop/personality/personality/combined_edges.csv"
write_csv(combined_df, combined_output)


# ------------------------------------------------------------------
# Analysis for total_time_at_edge -----------------------------------
# 1. Data for first trials (T1 and T1S1) grouped by ID, task, season
first_trials <- combined_df %>% 
  filter(trial %in% c("T1", "T1S1")) %>% 
  group_by(ID, task, season) %>% 
  summarise(mean_percent_time_at_edge = mean(percent_time_at_edge, na.rm = TRUE), .groups = "drop")
print("First trials data:")
print(first_trials)

# 2. All data for IDs with more than one season and/or more than one task
multi_season_task_IDs <- combined_df %>% 
  group_by(ID) %>% 
  summarise(n_seasons = n_distinct(season), n_tasks = n_distinct(task)) %>% 
  filter(n_seasons > 1 | n_tasks > 1) %>% 
  pull(ID)
multi_data <- combined_df %>% filter(ID %in% multi_season_task_IDs)
print("Data for IDs with multiple seasons/tasks:")
print(multi_data)

# 3. Best plot: Trends for all IDs - faceted scatter plot by ID, colored by season, shaped by task
# Order trials
trial_levels <- c("T1", "T1S1", "T2", "T1S2", "T3", "T2S1", "T4", "T2S2", "T5", "T6", "T7", "T8", "T9", "T10")
combined_df <- combined_df %>% mutate(trial = factor(trial, levels = trial_levels))
p_trends <- ggplot(combined_df, aes(x = trial, y = percent_time_at_edge, color = season, shape = task)) +
  geom_point(position = position_jitter(width = 0.1)) +
  facet_wrap(~ ID, scales = "free_y") +
  labs(title = "Trends in Percent Time at Edge by ID", x = "Trial", y = "Percent Time at Edge", color = "Season", shape = "Task") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(p_trends)

# 4. Visualizations for 1,2,3 with T10 last in order
# For first_trials
p6 <- ggplot(first_trials, aes(x = season, y = mean_percent_time_at_edge, fill = task)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Mean Percent Time at Edge for First Trials", x = "Season", y = "Mean percent Time at Edge", fill = "Task") +
  theme_minimal()
print(p6)

# For multi_data
multi_data <- multi_data %>% mutate(trial = factor(trial, levels = trial_levels))
p7 <- ggplot(multi_data, aes(x = trial, y = percent_time_at_edge, color = season, shape = task)) +
  geom_point(position = position_jitter(width = 0.1)) +
  scale_x_discrete(limits = trial_levels) +
  labs(title = "Percent Time at Edge for Multi-Season/Task IDs", x = "Trial", y = "Percent Time at Edge", color = "Season", shape = "Task") +
  theme_minimal()
print(p7)

# Save these plots
ggsave("total_time_boxplot.png", plot = p5, width = 8, height = 6)
ggsave("first_trials_bar.png", plot = p6, width = 8, height = 6)
ggsave("multi_data_scatter.png", plot = p7, width = 10, height = 6)

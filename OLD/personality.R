library(tidyverse)
library(utils)
library(sf) 
library(mapview) 
library(parallel)
library(ggplot2)
library(trajr)
library(brms)
library(gifski)
library(png)
library(transformr)
library(gganimate)
library(dplyr)
library(forcats)
library(readr)
library(mgcv)
library(car)

#latency
setwd("/Users/narctaz/Documents/personality/pilot")
data <- read.csv("emergence.csv")
data <- data %>%
  mutate(id = as.factor(id),
         season = as.factor(season),
         n_trial = as.factor(n_trial),
         trial = paste0("T", n_trial),
         trial = as.factor(trial),
         unique_trial_ID = as.factor(paste(season, id, trial, sep = "_")))
#data <- data %>% drop_na(habituation, emergence)
data$n_trial <- NULL  
data$emergence <- as.character(data$emergence)
data$emergence[data$emergence == "<1"] <- "0.5"
data$emergence <- as.numeric(data$emergence)

data$habituation <- as.character(data$habituation)
data$habituation[data$habituation == "<1"] <- "0.5"
data$habituation <- as.numeric(data$habituation)

#T1
first_trials <- data %>% filter(trial == "T1")
median_values <- first_trials %>%
  summarise(
    median_habituation = median(habituation, na.rm = TRUE),
    median_emergence = median(emergence, na.rm = TRUE)
  )

#plots
plot_data <- data.frame(
  Measure = c("Habituation", "Emergence"),
  Median = c(median_values$median_habituation, median_values$median_emergence)
)

ggplot(plot_data, aes(x = Measure, y = Median, fill = Measure)) +
  geom_bar(stat = "identity") +
  labs(title = "Median Values for First Trials",
       x = "Measure",
       y = "Median Value") +
  theme_minimal() +
  scale_fill_manual(values = c("Habituation" = "blue", "Emergence" = "red"))

# Calculate the median for habituation and emergence by individual
median_by_season <- first_trials %>%
  group_by(season) %>%
  summarise(
    median_habituation = median(habituation, na.rm = TRUE),
    median_emergence = median(emergence, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = starts_with("median_"),
               names_to = "Measure",
               values_to = "Median") %>%
  mutate(Measure = gsub("median_", "", Measure))
# Plot the median values by individual
ggplot(median_by_season, aes(x = season, y = Median, fill = Measure)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Comparison of Median Values Between Individuals",
       x = "Individual",
       y = "Median Value") +
  theme_minimal() +
  scale_fill_manual(values = c("habituation" = "darkgoldenrod3", "emergence" = "red"))

# Summarize the data to calculate median values by individual and season
median_by_individual_season <- first_trials %>%
  group_by(id, season) %>%
  summarise(
    median_habituation = median(habituation, na.rm = TRUE),
    median_emergence = median(emergence, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = starts_with("median_"),
               names_to = "Measure",
               values_to = "Median") %>%
  mutate(Measure = gsub("median_", "", Measure))

# Plot the median values by individual and season
ggplot(median_by_individual_season, aes(x = id, y = Median, fill = Measure)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Comparison of Median Values Between Individuals by Season",
       x = "Individual",
       y = "Median Value") +
  theme_minimal() +
  scale_fill_manual(values = c("habituation" = "darkgoldenrod3", "emergence" = "red")) +
  facet_wrap(~season, ncol = 1)

ggplot(median_by_individual_season, aes(x = id, y = Median, color = season)) +
  geom_boxplot(outlier.shape = NA) +  # Box plot without outliers (since jitter will show them)
  #geom_jitter(position = position_jitter(width = 0.2, height = 0), size = 2, alpha = 0.7) +  # Jittered points
  labs(title = "Box Plot of Median latency by Trial ID with season",
       x = "ID",
       y = "Median",
       color = "season") +
  scale_color_manual(values = c("summer" = "orange", "winter" = "purple", "spring" = "blue")) + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#data$season <- trimws(data$season)
#data$season <- fct_collapse(data$season,
#spring = "spring",
#winter = "winter",
#summer = c("summer", "summer "))


ggplot(data = data) + 
  #geom_point(mapping = aes(x = trial, y = emergence, color = season))+
  #geom_smooth(mapping = aes(x = trial, y = emergence, color = season))+
  geom_point(mapping = aes(x = trial, y = emergence, color = season)) +
  scale_color_manual(values = c("green", "coral", "cornflowerblue")) + 
  facet_wrap(~id)

# Calculate mean values for each ID
mean_data <- data %>%
  group_by(id, season) %>%
  summarize(mean_values = mean(emergence))

# Plotting mean values by ID and season
ggplot(mean_data, aes(x = id, y = mean_values, fill = season)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(x = "id", y = "Mean emergence's latency", title = "Mean emergence's latency by id and season") +
  scale_fill_manual(values = c("spring" = "green", "summer" = "coral", "winter" = "cornflowerblue")) +
  theme_minimal()

###

#exploration
distance <- read_csv("/Users/narctaz/Desktop/personality/cue/results/cleaned_distance.csv", col_types = cols(.default = "c"))
#fixing the dataset
#distance$season <- as.character(distance$season)
#sum(rows_to_fix)  

#rows_to_fix <- distance$season == "1"
#distance[rows_to_fix, 1:(ncol(distance)-1)] <- distance[rows_to_fix, 2:ncol(distance)]  
#distance[rows_to_fix, ncol(distance)] <- NA

#write.csv(distance, "/Users/narctaz/Desktop/personality/cue/results/cleaned_distance.csv", row.names = FALSE)
##
distance <- distance %>%
  mutate(
    food_door = as.numeric(food_door), 
    walked_to = as.numeric(walked_to), 
    walked_back = as.numeric(walked_back), 
    walk_exploration = as.numeric(walk_exploration), 
    straightness_exploration = as.numeric(straightness_exploration)
  )

distance <- distance %>%
  mutate(walk_exploration = ifelse(is.na(walk_exploration), 0, walk_exploration))

distance$walk_exploration_label <- ifelse(is.na(distance$walk_exploration), "No Exploration", "Exploration")
distance <- distance %>%
  mutate(unique_trial_id = paste(season, ID, trial, sep = "_"))

#plots
ggplot(distance, aes(x = walk_exploration, y = status, color = as.factor(status))) +
  geom_point() +
  labs(title = "Scatter Plot of Walk Exploration vs Status", x = "Walk Exploration", y = "Status") +
  theme_minimal()

ggplot(distance, aes(x = walk_exploration_label, y = unique_trial_id, color = as.factor(status))) +
  geom_point() +
  labs(title = "Scatter Plot of Walk Exploration label vs id", x = "Walk Exploration", y = "Status") +
  theme_minimal()

#statistics
shapiro.test(distance$walk_exploration)

anova_result <- aov(walk_exploration ~ status, data = distance)
summary(anova_result)

# post hoc
tukey_result <- TukeyHSD(anova_result)
print(tukey_result)
###
distance <- distance %>%
  mutate(individual_id = paste(season, ID, sep = "_"), 
         trial = factor(trial, levels = c("T1", "T2", "T3", "T4", "T5", "T6", "T7", "T8", "T9", "T10")))
linear_model <- lm(walk_exploration ~ trial * individual_id, data = distance)
summary(linear_model)

ggplot(distance, aes(x = trial, y = walk_exploration, color = individual_id)) +
  geom_point() +                        # Points for each observation
  geom_smooth(method = "lm", se = FALSE) +  # Linear regression lines
  labs(title = "Walk Exploration Across Trials",
       x = "Trial",
       y = "Walk Exploration",
       color = "Individual") +
  theme_minimal()

#plot 2
ggplot(distance, aes(x = trial, y = walk_exploration, group = individual_id, color = individual_id)) +
  geom_point(size = 3) +                         
  geom_line(size = 1) +                         
  labs(title = "Walk Exploration Across Trials",
       x = "Trial",
       y = "Walk Exploration",
       color = "Individual") +
  theme_minimal() +
  theme(legend.position = "top")   

ggplot(distance, aes(x = trial, y = walk_exploration, group = individual_id, color = status)) +
  geom_point(size = 3) +                         # Points for each observation
  geom_line(size = 1) +                          # Lines connecting the points
  labs(title = "Walk Exploration Across Trials",
       x = "Trial",
       y = "Walk Exploration",
       color = "Individual") +
  theme_minimal() +
  theme(legend.position = "top")   

#mixed-effects model
# Fit a linear mixed-effects model
model <- lmer(walk_exploration ~ trial + (1 | individual_id), data = distance)
summary(model)# Check residuals
plot(resid(model) ~ fitted(model))
abline(h = 0, col = "red")

ggplot(distance, aes(x = trial, y = walk_exploration)) +
  geom_point()+
  geom_smooth(method = "lm")

# Fit a GAM model
gam_model <- gam(walk_exploration ~ s(trial) + s(individual_id), data = distance)
summary(gam_model)

# Check for influential observations
influence <- influence(model)
plot(influence)

# Check VIF
vif_model <- lm(walk_exploration ~ trial + status, data = distance)
vif(vif_model)

install.packages("car")
install.packages("influence.ME")
library(influence.ME)
# Calculate influence
influence_me <- influence(model, group = "individual_id")
plot(influence_me)

# Calculate residuals and fitted values
residuals <- resid(model)
fitted_values <- fitted(model)

# Calculate Cook's Distance
cooks_dist <- cooks.distance(model)

# Create a plot for Cook's Distance
plot(cooks_dist, type = "h", main = "Cook's Distance", ylab = "Distance", xlab = "Observations")
abline(h = 4 / length(cooks_dist), col = "red")
###

#ratio
#folder_path <- "/Users/narctaz/Documents/cue_R/R/data/"  
#files <- list.files(path = folder_path, pattern = "*.csv", full.names = TRUE)
#data_list <- lapply(files, read.csv)
#merged_data <- do.call(rbind, data_list)
#write.csv(merged_data, file.path(folder_path, "merged.csv"), row.names = FALSE)
#merged_data$Ratio <- merged_data$at_edge_cm / merged_data$out_edge_cm
#head(merged_data)
#write.csv(merged_data, "ratio.csv", row.names = FALSE)

ratio <- read.csv("/Users/narctaz/Documents/cue_R/R/data/merged_with_ratio.csv")
ratio <- ratio %>%
  mutate(unique_trial_ID = paste(season, ID, trial, sep = "_"))


ggplot(ratio, aes(x = unique_trial_ID, y = Ratio)) +
  geom_point() +  # Create scatterplot points
  labs(title = "Scatterplot of Ratio by unique_trial_id", 
       x = "Unique_trial_ID", 
       y = "Ratio") +
  theme_minimal()  # Use a clean theme

# Filter the data to include only trials T1 and T10
filtered_data <- ratio[ratio$trial %in% c("T9", "T10"), ]

ggplot(filtered_data, aes(x = unique_trial_ID, y = Ratio, color = trial)) +
  geom_point() +  # Create scatterplot points
  labs(title = "Scatterplot of Ratio by Unique Trial ID for Trials T1 and T10",
       x = "Unique Trial ID",
       y = "Ratio",
       color = "Trial") +  # Add legend for trial color
  theme_minimal()

#season
filtered_data <- ratio[ratio$season %in% c("winter", "summer"), ]

ggplot(filtered_data, aes(x = unique_trial_ID, y = Ratio, color = season)) +
  geom_point() +  # Create scatterplot points
  scale_color_manual(values = c("summer" = "darkgoldenrod3",
                                "winter" = "cornflowerblue")) +
  labs(title = "Scatterplot of Ratio by season",
       x = "Unique Trial ID",
       y = "Ratio",
       color = "season") +  # Add legend for trial color
  theme_minimal()
#try captive vs wild
ratio <- ratio %>%
  mutate(season_ID = paste(season, ID, sep = "_"))

# isn't working
captive_ids <- ratio %>%
  filter(season %in% c("winter", "summer")) %>%  # Keep only Winter and Summer
  group_by(trial_ID) %>%                  # Group by unique_trial_id
  filter(n_distinct(season) > 1) %>%             # Keep those present in both seasons
  pull(trial_ID) %>%                      # Extract unique_trial_ids
  unique()
captive_ids <- captive_ids[captive_ids %in% ratio$trial_ID[ratio$season == "summer"]]

ratio <- ratio %>%
  mutate(capture_status = ifelse(trial_ID %in% captive_ids, "captive", "wild"))
####
captive_ids <- c("winter_20200625-3", "winter_20200804-2")
semicaptive_ids <- c("winter_20201101-1", "winter_20201031-1", "winter_20201101-3", "winter_20201103-1", "winter_20201103-2", "winter_20201103-3", "winter_20201103-4", "winter_20201103-5", "wonter_20201106-1", "winter_20201106-2")
ratio <- ratio %>%
  mutate(capture_status = ifelse(season_ID %in% captive_ids, "captive","wild"))

ratio <- ratio %>%
  mutate(capture_status = case_when(
    season_ID %in% captive_ids ~ "captive",       # If the season_ID is in captive_ids, mark as "captive"
    season_ID %in% semicaptive_ids ~ "semicaptive",  # If the season_ID is in semicaptive_ids, mark as "semicaptive"
    TRUE ~ "wild"                                 # Otherwise, mark as "wild"
  ))

ggplot(ratio, aes(x = season, y = Ratio, color = capture_status)) +
  geom_point() +  
  scale_color_manual(values = c("wild" = "yellow",
                                "captive" = "black",
                                "semicaptive" = "deepskyblue4")) +
  labs(title = "Scatterplot of Ratio by capture status",
       x = "season",
       y = "Ratio",
       color = "capture_status") +  # Add legend for trial color
  theme_minimal()
theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(ratio, aes(x = season, y = Ratio, color = capture_status)) +
  geom_boxplot(outlier.shape = NA) +  # Box plot without outliers (since jitter will show them)
  #geom_jitter(position = position_jitter(width = 0.2, height = 0), size = 2, alpha = 0.7) +  # Jittered points
  labs(title = "Box Plot of Ratio by Trial ID with Captive, Semicaptive, and Wild IDs",
       x = "season",
       y = "Ratio",
       color = "Capture Status") +
  scale_color_manual(values = c("captive" = "orange", "semicaptive" = "purple", "wild" = "blue")) +  # Custom colors
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#individual plots

for(id in unique(ratio$ID)) {
  
  ratio_filtered <- ratio %>%
    filter(ID == id)
  
  p <- ggplot(ratio, aes(x = trial, y = Ratio, fill = capture_status)) +
    geom_boxplot(outlier.shape = NA, alpha = 0.6) +  # Box plot with transparency, no outliers
    geom_jitter(position = position_jitter(width = 0.2, height = 0), size = 1.5, alpha = 0.6) +  # Jitter points
    labs(title = paste("Box Plot of Ratio by Trial for", id),
         x = "Trial",
         y = "Ratio",
         fill = "Capture Status") +
    scale_fill_manual(values = c("captive" = "orange", "semicaptive" = "purple", "wild" = "blue")) +  # Custom colors
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  
  ggsave(filename = paste0("boxplot_", id, ".png"), plot = p, width = 8, height = 6, dpi = 300)
  
}

ggplot(ratio, aes(x = trial, y = Ratio, fill = capture_status)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +  # Box plot with transparency, no outliers
  geom_jitter(position = position_jitter(width = 0.2, height = 0), size = 1.5, alpha = 0.6) +  # Jitter points to avoid overlap
  labs(title = "Box Plot of Ratio by Trial and Capture Status",
       x = "Trial",
       y = "Ratio",
       fill = "Capture Status") +
  scale_fill_manual(values = c("captive" = "orange", "semicaptive" = "purple", "wild" = "blue")) + 
  facet_wrap(~ ID, scales = "free_x") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


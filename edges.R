library(tidyverse)
library(sf)
library(here)

# Cue - Edges calculation ----
cue_foodjourney <- read_csv(here("cue/master_results.csv")) %>% 
  mutate(unique_trial_ID = as.factor(unique_trial_ID),
         season = as.factor(season),
         ID = as.factor(ID),
         food_journey = as.factor(food_journey))
cue_ls <- split(cue_foodjourney, cue_foodjourney$unique_trial_ID)
cue_doors <- read.csv(here("cue/trial_door.csv"))  %>%
  mutate(trial = paste0("T",trial_n))

cue_coords <- read_csv(here("cue/coords.csv"))
cue_edges <- here("cue/edges.csv")

cue_edge_pr <- lapply(cue_ls, function(x){
  x <- cue_ls[[196]]
  season_filter <- if (any(str_starts(x$unique_trial_ID, "winter_2024"))) {
    "winter_2024"
  } else {
    "other"
  }
  track_sf <- x %>%
    st_as_sf(coords = c("x", "y")) %>% 
    mutate(x = st_coordinates(.)[, 1],
           y = st_coordinates(.)[, 2])
  trial_door_ID <- cue_doors %>%
    filter(trial == unique(x$trial)) %>%
    filter(season == season_filter) %>%
    pull(door)
  
  doors_x <- cue_coords %>%
    filter(unique_trial_ID == unique(x$unique_trial_ID)) %>%
    select(4:11) %>%
    pivot_longer(cols = contains("x"), names_to = "door", values_to = "x") %>%
    select(x)
  cue_coords <- cue_coords %>%
    filter(unique_trial_ID == unique(x$unique_trial_ID)) %>%
    select(4:11) %>%
    pivot_longer(cols = contains("y"), names_to = "door", values_to = "y") %>%
    mutate(door_ID = substr(door,1,1)) %>%
    select(c("door_ID", "y")) %>%
    bind_cols(doors_x)
  
  all_doors_buffer <- cue_coords %>%
    st_as_sf(coords = c("x","y")) %>%
    st_buffer(dist = 4)
  trial_door_buffer <- all_doors_buffer %>%
    filter(door_ID == trial_door_ID)
  
  # Define side length
  side_length <- 114
  
  # Function to calculate intersection of two lines
  calculate_intersection <- function(x1, y1, x2, y2, x3, y3, x4, y4) {
    if (any(is.na(c(x1, y1, x2, y2, x3, y3, x4, y4)))) {
      return(c(NA, NA))
    }
    denom <- (y4 - y3) * (x2 - x1) - (x4 - x3) * (y2 - y1)
    if (length(denom) == 0 || denom == 0) {
      return(c(NA, NA)) # This is for parallel lines
    }
    ua <- ((x4 - x3) * (y1 - y3) - (y4 - y3) * (x1 - x3)) / denom
    x <- x1 + ua * (x2 - x1)
    y <- y1 + ua * (y2 - y1)
    return(c(x, y))
  }
  
  # calculate perpendicular vector
  calculate_perpendicular_vector <- function(x1, y1, x2, y2) {
    if (any(is.na(c(x1, y1, x2, y2)))) {
      return(c(0, 0))
    }
    dx <- x2 - x1
    dy <- y2 - y1
    mag <- sqrt(dx^2 + dy^2)
    if (length(mag) == 0 || mag == 0) {
      return(c(0, 0)) # Avoid division by zero
    }
    unit_vector <- c(dx / mag, dy / mag)
    # Perpendicular components
    perp_dx <- -unit_vector[2]
    perp_dy <- unit_vector[1]
    return(c(perp_dx, perp_dy))
  }
  
  # Ensure cue_coords has the required rows
  if (nrow(cue_coords) >= 4) {
    # Intersection of diagonals A-C and B-D
    intersection <- calculate_intersection(cue_coords$x[1], cue_coords$y[1], cue_coords$x[3], cue_coords$y[3], # A-C
                                           cue_coords$x[2], cue_coords$y[2], cue_coords$x[4], cue_coords$y[4])  # B-D

    # Check if intersection is valid
    if (any(is.na(intersection))) {
      stop("Intersection of diagonals is invalid.")
    }
}
    # Find endpoints with perpendiculars and stop at intersections
  corner_points <- list()
  for (i in 1:nrow(cue_coords)) {
      next_i <- ifelse(i == nrow(cue_coords), 1, i + 1)
      
      center_x <- cue_coords$x[i]
      center_y <- cue_coords$y[i]
      next_center_x <- cue_coords$x[next_i]
      next_center_y <- cue_coords$y[next_i]
      
      # Perpendicular vector for current door
      perp_vector <- calculate_perpendicular_vector(center_x, center_y, intersection[1], intersection[2])
      
      # Potential endpoints for the current side
      shift <- side_length / 2
      x1 <- center_x - shift * perp_vector[1]
      y1 <- center_y - shift * perp_vector[2]
      x2 <- center_x + shift * perp_vector[1]
      y2 <- center_y + shift * perp_vector[2]
      
      # Intersection with the next side perpendicular line
      next_perp_vector <- calculate_perpendicular_vector(next_center_x, next_center_y, intersection[1], intersection[2])
      x3 <- next_center_x - shift * next_perp_vector[1]
      y3 <- next_center_y - shift * next_perp_vector[2]
      x4 <- next_center_x + shift * next_perp_vector[1]
      y4 <- next_center_y + shift * next_perp_vector[2]
      
      intersection_point <- calculate_intersection(x1, y1, x2, y2, x3, y3, x4, y4)
      
      if (!any(is.na(intersection_point))) {
        corner_points[[i]] <- intersection_point
      } else {
        corner_points[[i]] <- c(x2, y2) # Second endpoint as fallback
      }
    }
    
    # Convert corner points to data frame
    corner_points_df <- do.call(rbind, corner_points)
    
    # Check if corner points are valid
    if (nrow(corner_points_df) != 4) {
      stop("Corner points calculation failed.")
    }
    
    # Create multiline string and spatial features
    side_lines <- st_multilinestring(list(
      rbind(corner_points_df[1,], corner_points_df[2,]),
      rbind(corner_points_df[2,], corner_points_df[3,]),
      rbind(corner_points_df[3,], corner_points_df[4,]),
      rbind(corner_points_df[4,], corner_points_df[1,])
    )) %>% st_sfc() %>% st_sf()
    
    # Create buffer around the side lines
    buffer_distance <- 4 
    edges_buffer <- st_buffer(side_lines, dist = buffer_distance)
    
    # at_edge <- track_sf %>%
    #   st_intersection(edges_buffer) %>%
    #   as.data.frame() %>%
    #   arrange(frame) #arrange by time/frame
    track_sf <- track_sf %>%
      mutate(at_edge = as.vector(st_intersects(geometry, edges_buffer, sparse = FALSE)))
    
    time_at_edge <- track_sf %>%
      filter(at_edge == TRUE) %>% {
        if (nrow(.) == 0) {
          tibble(total_time = 0)
        } else {
          summarize(., total_time = sum(time)) }
      } %>%
      st_drop_geometry()
    
    time_out_edge <- track_sf %>%
      filter(!at_edge) %>% {
        if (nrow(.) == 0) {
          tibble(total_time = 0)
        } else {
          summarize(., total_time = sum(time)) }
      } %>%
      st_drop_geometry()
    
    calculate_distance <- function(data) {
      data <- data %>%
        st_drop_geometry() %>%
        arrange(frame) %>%
        mutate(x_lag = lag(x),
               y_lag = lag(y),
               dist = sqrt((x - x_lag)^2 + (y - y_lag)^2))
      total_distance <- sum(data$dist, na.rm = TRUE)
      return(total_distance)
    }
    
    length_edge <- calculate_distance(track_sf %>% filter(at_edge == TRUE))
    length_center <- calculate_distance(track_sf %>% filter(at_edge == FALSE))
    length_edge_tripto <- calculate_distance(track_sf %>% filter(food_journey == "trip_to" & at_edge == TRUE))
    length_edge_tripback <- calculate_distance(track_sf %>% filter(food_journey == "trip_back" & at_edge == TRUE))
    length_center_tripto <- calculate_distance(track_sf %>% filter(food_journey == "trip_to" & at_edge == FALSE))
    length_center_tripback <- calculate_distance(track_sf %>% filter(food_journey == "trip_back" & at_edge == FALSE))
    
    time_edge <- time_at_edge$total_time
    time_center <- time_out_edge$total_time
    time_total <- time_edge + time_center
    total_path_length <- length_edge + length_center
    prop_time_edge <- ifelse(time_total > 0, time_edge / time_total, NA)
    prop_path_length_edge <- ifelse(total_path_length > 0, length_edge / total_path_length, NA)
    
    df <- data.frame(task = "cue", 
                     x[1,1],x[1,2],x[1,3], x[1,4],
                     time_total = time_total, time_edge = time_edge, time_center = time_center,
                     prop_time_edge = prop_time_edge,
                     path_length_edge = length_edge, path_length_center = length_center, prop_path_length_edge = prop_path_length_edge,
                     path_length_edge_trip_to = length_edge_tripto, path_length_edge_trip_back = length_edge_tripback, path_length_center_trip_to = length_center_tripto, path_length_center_trip_back = length_center_tripback) %>% droplevels()
    
    write.table(df, file = cue_edges, append = TRUE, sep = ",", row.names = FALSE, col.names = !file.exists(cue_edges))
  }
)

# Foraging - Edges calculation ####

foraging_tracking <- read_csv(here("foraging/all_trials.csv")) %>% 
  mutate(ID = as.factor(ID),
         season = as.factor(season),
         trial = as.factor(trial),
         unique_trial_ID = as.factor(paste(season, ID, trial, sep = "_"))) %>% 
  relocate(unique_trial_ID, .before = ID)

# cm per pixel in foraging = 0.187192
fr_cm_per_pixel = 0.187192

fr_coords <- read_csv(here("foraging/islands.csv")) %>%
  separate(A, into = c("A_x", "A_y"), sep = ";", convert = TRUE) %>% 
  separate(B, into = c("B_x", "B_y"), sep = ";", convert = TRUE) %>% 
  separate(C, into = c("C_x", "C_y"), sep = ";", convert = TRUE) %>% 
  separate(D, into = c("D_x", "D_y"), sep = ";", convert = TRUE) %>% 
  separate(door, into = c("door_x", "door_y"), sep = ";", convert = TRUE) %>% 
  mutate(unique_trial_ID = as.factor(paste(season, ID, trial, sep = "_")),
         ID = as.factor(ID),
         season = as.factor(season),
         trial = as.factor(trial),
         across(contains("_x"), ~ . * fr_cm_per_pixel),
         across(contains("_y"), ~ . * fr_cm_per_pixel))

fr_corners <- read_csv(here("foraging/foraging_corners.csv")) %>% 
  mutate(ID = as.factor(ID),
         season = as.factor(season),
         trial = as.factor(trial),
         unique_trial_ID = as.factor(paste(season, ID, trial, sep = "_")),
         across(contains("_x"), ~ . * fr_cm_per_pixel),
         across(contains("_y"), ~ . * fr_cm_per_pixel)) %>%
  relocate(unique_trial_ID, .before = ID)

#check mismatch in unique_trial_ID.length between foraging_coords (and therefore foraging_tracking) and foraging_corners
#returns ID that are in foraging_corners but are missing from foraging_coords
setdiff(unique(foraging_corners$unique_trial_ID), unique(foraging_coords$unique_trial_ID))
#there are 12 unique_trial_IDs that are missing from foraging_coords - those experiments were never ran (e.g. shrew 20210802-3 did not do any trial, while some are missing one or two trials)
valid_ids <- unique(foraging_coords$unique_trial_ID)

fr_corners <- fr_corners %>%
  filter(unique_trial_ID %in% valid_ids) %>%
  droplevels()

foraging_ls <- split(foraging_tracking, foraging_tracking$unique_trial_ID)


foraging_edges <- here("foraging/foraging_edges.csv")

fr_edges_pr <- lapply(foraging_ls, function(x){
  #x <- foraging_ls[[1]]
  
  track_sf <- x %>%
    st_as_sf(coords = c("x", "y")) %>% 
    mutate(x = st_coordinates(.)[, 1],
           y = st_coordinates(.)[, 2])
  doors_x <- fr_coords %>%
    filter(unique_trial_ID == unique(x$unique_trial_ID)) %>%
    select("door_x")
    
  fr_door <- fr_coords %>%
    filter(unique_trial_ID == unique(x$unique_trial_ID)) %>%
    select("door_y") %>% 
    bind_cols(doors_x)
  
  door_buffer <- fr_doors %>%
    st_as_sf(coords = c("door_x","door_y")) %>%
    st_buffer(dist = 4)
  
  #define corners for trial x
  corners <- fr_corners %>%
    filter(unique_trial_ID == unique(x$unique_trial_ID)) %>%
    select(topleft_x, topleft_y, topright_x, topright_y, downright_x, downright_y, downleft_x, downleft_y)
  
  # Build a matrix of corner coordinates in order:
  corner_coords <- rbind(
    c(corners$topleft_x, corners$topleft_y),
    c(corners$topright_x, corners$topright_y),
    c(corners$downright_x, corners$downright_y),
    c(corners$downleft_x, corners$downleft_y),
    c(corners$topleft_x, corners$topleft_y) # close the polygon
  )
  
  side_lines_sfc <- st_sfc(st_linestring(corner_coords))
  side_lines_sf <- st_sf(geometry = side_lines_sfc)
  
  # Create buffer around arena edges
  buffer_distance <- 4
  edges_buffer <- st_buffer(side_lines_sf, dist = buffer_distance)
  
  track_sf <- track_sf %>%
    mutate(at_edge = as.vector(st_intersects(geometry, edges_buffer, sparse = FALSE)))
  
  time_at_edge <- track_sf %>%
    filter(at_edge == TRUE) %>% {
      if (nrow(.) == 0) {
        tibble(total_time = 0)
      } else {
        summarize(., total_time = sum(time)) }
    } %>%
    st_drop_geometry()
  
  time_out_edge <- track_sf %>%
    filter(!at_edge) %>% {
      if (nrow(.) == 0) {
        tibble(total_time = 0)
      } else {
        summarize(., total_time = sum(time)) }
    } %>%
    st_drop_geometry()
  
  calculate_distance <- function(data) {
    data <- data %>%
      st_drop_geometry() %>%
      arrange(frame) %>%
      mutate(x_lag = lag(x),
             y_lag = lag(y),
             dist = sqrt((x - x_lag)^2 + (y - y_lag)^2))
    total_distance <- sum(data$dist, na.rm = TRUE)
    return(total_distance)
  }
  
  time_edge <- time_at_edge$total_time
  time_center <- time_out_edge$total_time
  time_total <- time_edge + time_center
  
  prop_time_edge <- ifelse(time_total > 0, time_edge / time_total, NA)
  
  path_length_edge <- track_sf %>%
    filter(at_edge == TRUE) %>%
    calculate_distance()
  
  path_length_center <- track_sf %>%
    filter(at_edge == FALSE) %>%
    calculate_distance()
  
  total_path_length <- path_length_edge + path_length_center
  
  prop_path_length_edge <- ifelse(total_path_length > 0,
                                  path_length_edge / total_path_length,
                                  NA)
  df <- data.frame(task = "foraging", x[1,1], x[1,2], x[1,3], x[1,4],
                   time_total = time_total, time_edge = time_edge, time_center = time_center, prop_time_edge = prop_time_edge,
                   path_length_edge = path_length_edge, path_length_center = path_length_center, prop_path_length_edge = prop_path_length_edge) %>% 
    droplevels()
  
  
  write.table(df, file = foraging_edges, append = TRUE, sep = ",", row.names = FALSE, col.names = !file.exists(foraging_edges))
  
}
)

library(dplyr)
library(sf)
# I am adding stuff in the git cloud 
# Set working directory and read in the data
setwd("/Users/narctaz/Desktop/personality/cue")
#tracking <- read.csv("all_track.csv") 
#tracking[c('ANGLE')][sapply(tracking[c('ANGLE')], is.infinite)] <- NA 
#tracking <- tracking %>% drop_na(ANGLE)

tracking_foodjourney <- read_csv("/Users/narctaz/Desktop/personality/cue/results/master_results.csv") %>% 
  mutate(unique_trial_ID = as.factor(unique_trial_ID),
         season = as.factor(season),
         
         ID = as.factor(ID),
         food_journey = as.factor(food_journey))
trial_ls2 <- split(tracking_foodjourney, tracking_foodjourney$unique_trial_ID)
doors <- read.csv("/Users/narctaz/Desktop/personality/cue/trial_door.csv", sep = ",", header = TRUE)  %>%
  mutate(trial = paste0("T",trial_n))
#mutate(Trial = paste0("T", trial))

coords <- read_csv("coords.csv")
#doors_coords <- read.csv("food_door_coordinates.csv") %>%
  #mutate(Trial = paste0("T", TRIAL)) %>% 
  #mutate(unique_trial_ID = paste(SEASON, ID, TRIAL, sep = "_"))

edges <- "/Users/narctaz/Desktop/personality/cue/results/edges.csv"
edge_pr <- lapply(trial_ls2, function(x){
  x <- trial_ls2[[1]]
  season_filter <- if (any(str_starts(x$unique_trial_ID, "winter_2024"))) {
    "winter_2024"
  } else {
    "other"
  }
  track_sf <- x %>%
    st_as_sf(coords = c("x", "y"))
  trial_door_ID <- doors %>%
    filter(trial == unique(x$trial)) %>%
    filter(season == season_filter) %>%
    pull(door)
  
  doors_x <- coords %>%
    filter(unique_trial_ID == unique(x$unique_trial_ID)) %>%
    select(4:11) %>%
    pivot_longer(cols = contains("x"), names_to = "door", values_to = "x") %>%
    select(x)
  coords <- coords %>%
    filter(unique_trial_ID == unique(x$unique_trial_ID)) %>%
    select(4:11) %>%
    pivot_longer(cols = contains("y"), names_to = "door", values_to = "y") %>%
    mutate(door_ID = substr(door,1,1)) %>%
    select(c("door_ID", "y")) %>%
    bind_cols(doors_x)
  
  all_doors_buffer <- coords %>%
    st_as_sf(coords = c("x","y")) %>%
    st_buffer(dist = 4)
  trial_door_buffer <- all_doors_buffer %>%
    filter(door_ID == trial_door_ID)
  
  #doors_x <- doors_coords %>%
  #select(4:11, unique_trial_ID) %>%
  #pivot_longer(cols = contains("x"), names_to = "door", values_to = "x") 
  #mutate(door_ID = substr(door, 1, 1))
  
  #doors_y <- doors_coords %>%
  #select(4:11, unique_trial_ID) %>%
  # pivot_longer(cols = contains("y"), names_to = "door", values_to = "y") 
  #mutate(door_ID = substr(door, 1, 1))
  
  #doors_coords <- doors_x %>%
  #inner_join(doors_y, by = c("unique_trial_ID")) %>%
  #select(unique_trial_ID, x, y)
  
  
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
  
  # Function to calculate perpendicular vector
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
  
  # Ensure coords has the required rows
  # if (nrow(coords) >= 4) {
  #   # Intersection of diagonals A-C and B-D
  #   intersection <- calculate_intersection(coords$x[1], coords$y[1], coords$x[3], coords$y[3], # A-C
  #                                          coords$x[2], coords$y[2], coords$x[4], coords$y[4])  # B-D
  #   
  #   # Check if intersection is valid
  #   if (any(is.na(intersection))) {
  #     stop("Intersection of diagonals is invalid.")
  #   }
    
    # Find endpoints with perpendiculars and stop at intersections
  corner_points <- list()
  for (i in 1:nrow(coords)) {
      next_i <- ifelse(i == nrow(coords), 1, i + 1)
      
      center_x <- coords$x[i]
      center_y <- coords$y[i]
      next_center_x <- coords$x[next_i]
      next_center_y <- coords$y[next_i]
      
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
    
    at_edge <- track_sf %>%
      st_intersection(edges_buffer) %>%
      as.data.frame() %>%
      arrange(frame) #arrange by time/frame
    
    x <- x %>%
      mutate(at_edge = as.vector(st_intersects(track_sf, edges_buffer, sparse = FALSE)))
    
    time_at_edge <- x %>%
      filter(at_edge == TRUE) %>%
      summarize(total_time = sum(time))
    
    calculate_distance <- function(data) {
      data <- data %>%
        arrange(frame) %>%
        mutate(
          x_lag = lag(x),
          y_lag = lag(y),
          dist = sqrt((x - x_lag)^2 + (y - y_lag)^2)
        )
      total_distance <- sum(data$dist, na.rm = TRUE)
      return(total_distance)
    }
    
    at_edge_cm <- calculate_distance(x %>% filter(at_edge == TRUE))
    out_edge_cm <- calculate_distance(x %>% filter(at_edge == FALSE))
    
    df = data.frame(x[1,1],x[1,2],x[1,3], x[1,4],
                    time_at_edge, at_edge_cm, out_edge_cm) %>% 
      droplevels()
    colnames(df) <- c(
      "season", "ID", "trial", "status",
      "time_at_edge", "at_edge_cm", "out_edge_cm"
    )
    write.table(df, file = edges, append = TRUE, sep = ",", row.names = FALSE, col.names = !file.exists(edges))
    # write.csv(df, file = paste0("/User/narctaz/Desktop/personality/cue/data/distance/", unique(x$unique_trial_ID),".csv")) 
    # # Plot the arena with side buffer
    # plot(st_geometry(buffer), col = 'lightblue', border = NA, main = "Arena with Side Buffer")
    # plot(st_geometry(side_lines), col = 'transparent', border = 'black', add = TRUE)
    # points(corner_points_df[,1], corner_points_df[,2], pch=16, col="red", cex=1.5)
    # points(coords$x, coords$y, pch=16, col="blue", cex=1.5)
    # text(coords$x, coords$y, labels=coords$door_ID, pos=3, col="blue", cex=1.2)
    #legend("topright", legend=c("Center Points", "Corner Points", "Buffer"), col=c("blue", "red", "lightblue"), pch=16, bg="white")
    
    ## plot with buffer and shrew
    # ggplot() +
    #   geom_sf(data = track_sf, aes(geometry = geometry, color = "Track")) +
    #   geom_sf(data = edges_buffer, aes(geometry = geometry, fill = "Edges Buffer"), alpha = 0.3) +
    #   geom_sf(data = at_edge, aes(geometry = geometry, color = "At Edge")) +
    #   scale_color_manual(values = c("Track" = "blue", "At Edge" = "red")) +
    #   scale_fill_manual(values = c("Edges Buffer" = "gray"))
    # Add legend to the plot
    
    edge_to_cm <- calculate_distance(x %>% filter(food_journey == "trip_to" & at_edge == TRUE))
    edge_back_cm <- calculate_distance(x %>% filter(food_journey == "trip_back" & at_edge == TRUE))
    
    out_edge_to_cm <- calculate_distance(x %>% filter(food_journey == "trip_to" & out_edge == TRUE))
    out_edge_back_cm <- calculate_distance(x %>% filter(food_journey == "trip_back" & out_edge == TRUE))
    #if at_edge2$food_journey == "exploration" then... create edge_exploration. else return
  }
)

# Day 12 advent of code -- Rain Risk
#
# ------------------------------------------------------------------------------

# set up -----------------------------------------------------------------------
library(tidyverse)

# load data --------------------------------------------------------------------

directions <- read_table(paste0(here::here(), "/day12.txt"), col_names = FALSE)

directions <- directions%>%
  extract(X1, c("direction", "distance"),
          "(.)(\\d+)")

# solve the puzzle -------------------------------------------------------------
# part 1

# initialise the starting postions and direction
position_x <- 0
position_y <- 0
initial_direction <- 90

# used to convert compas direction to degrees
code_direction <- data.frame(compas_direction = c("N", "S", "E", "W"),
                            direction_degree = c(0, 180, 90, 270))

directions %>%
  mutate(distance = as.numeric(distance),
         distance = ifelse(direction == "L",-distance, distance), # if L subtract the distance
         angle_change = ifelse(direction %in% c("L", "R"), distance, 0), # find the change in angle if L or R
         distance = ifelse(direction %in% c("L", "R"), 0, distance), # no change in distance if L or R
         direction_degree = (initial_direction + cumsum(angle_change)) %% 360) %>% # what is the current direction in degrees
  left_join(code_direction) %>% # join degrees to get compas direction
  # find the x and y movements
  mutate(x_movement = case_when((direction == "F" & compas_direction == "E") | (direction == "E") ~ distance,
                                (direction == "F" & compas_direction == "W") | (direction == "W") ~ -1 * distance),
         y_movement = case_when((direction == "F" & compas_direction == "N") | (direction == "N") ~ distance,
                                (direction == "F" & compas_direction == "S") | (direction == "S") ~ -1 * distance)) %>%
  summarise(manhattan =abs(sum(x_movement, na.rm = TRUE)) + abs(sum(y_movement, na.rm = TRUE))) # calculate manhattan distance

 # part 2 -- the directions were actually how to move a waypoint

ship_and_waypoint <- directions %>%
  mutate(distance = as.numeric(distance),
         distance = ifelse(direction == "L",-distance, distance), # if L subtract the distance
         angle_change = ifelse(direction %in% c("L", "R"), distance, 0), # find the change in angle if L or R
         distance = ifelse(direction %in% c("L", "R"), 0, distance)) %>%  # no change in distance if L or R
  mutate(waypoint_x_movement = case_when(direction == "E" ~ distance,
                                         direction == "W" ~ -1 * distance,
                                         TRUE ~ 0),
         waypoint_y_movement = case_when(direction == "N" ~ distance,
                                         direction == "S" ~ -1 * distance,
                                         TRUE ~ 0)) %>%
  mutate(waypoint_x = ifelse(row_number() == 1, 1, 0),
         waypoint_y = ifelse(row_number() == 1, -10, 0))

# loop over to find position of waypoints

for (i in 2:nrow(ship_and_waypoint)){
  ship_and_waypoint$waypoint_x[i]  <- case_when(ship_and_waypoint$angle_change[i] == 0 ~ ship_and_waypoint$waypoint_x[i-1] + ship_and_waypoint$waypoint_x_movement[i],
                                                ship_and_waypoint$angle_change[i] == 90 || ship_and_waypoint$angle_change[i] == -270 ~ ship_and_waypoint$waypoint_y[i-1],
                                                ship_and_waypoint$angle_change[i] == 180 || ship_and_waypoint$angle_change[i] == -180 ~ -ship_and_waypoint$waypoint_x[i-1],
                                                ship_and_waypoint$angle_change[i] == 270 || ship_and_waypoint$angle_change[i] == -90 ~ -ship_and_waypoint$waypoint_y[i-1])

  ship_and_waypoint$waypoint_y[i]  <- case_when(ship_and_waypoint$angle_change[i] == 0 ~ ship_and_waypoint$waypoint_y[i-1] + ship_and_waypoint$waypoint_y_movement[i],
                                                ship_and_waypoint$angle_change[i] == 90 || ship_and_waypoint$angle_change[i] == -270 ~ -ship_and_waypoint$waypoint_x[i-1],
                                                ship_and_waypoint$angle_change[i] == 180 || ship_and_waypoint$angle_change[i] == -180 ~ -ship_and_waypoint$waypoint_y[i-1],
                                                ship_and_waypoint$angle_change[i] == 270 || ship_and_waypoint$angle_change[i] == -90 ~ ship_and_waypoint$waypoint_x[i-1])
}

# find the ships positions

ship_and_waypoint %>%
  mutate(boat_movement_x = ifelse(direction == "F", distance * waypoint_x, 0),
         boat_movement_y = ifelse(direction == "F", distance * waypoint_y, 0)) %>%
  filter(direction == "F") %>%
  mutate(ship_x = cumsum(distance * waypoint_x),
         ship_y = cumsum(distance * waypoint_y)) %>%
  tail(1) %>%
  mutate(manhattan = abs(ship_x) + abs(ship_y))







# Decription of script ---------------------------------------------------------
# Day 17 advent of code -- Conway cubes
# ------------------------------------------------------------------------------

# set up -----------------------------------------------------------------------
library(tidyverse)

#%_% load data --------------------------------------------------------------------

cubes <- read_table('day17.txt', col_names = FALSE)

cubes_transformed <- cubes %>%
  separate(X1, paste("col", 1:nchar(cubes$X1[1])), sep = "(?<=.)",extra = "drop")

# solve the puzzle

# part 1

# keep track of cubes that are active
active_cubes <- data.frame(x_active = ceiling(which(cubes_transformed == "#")/nrow(cubes_transformed))) %>%
  mutate(y_active = which(cubes_transformed == "#") - (x_active-1)*nrow(cubes_transformed),
         z_active = 0)


for (reps in 1:6) { # number of times to iterate through
  # consider all of the neighbouring points
  all_points <- expand_grid(x = (min(active_cubes$x_active)-1):(max(active_cubes$x_active)+1),
                            y = (min(active_cubes$y_active)-1):(max(active_cubes$y_active)+1),
                            z = (min(active_cubes$z_active)-1):(max(active_cubes$z_active)+1))

  # this is all 26 of the neighbours that will be checked for each point
  neighbours <- expand_grid(x_neighbour = -1:1, y_neighbour = -1:1, z_neighbour = -1:1) %>%
    filter(!(x_neighbour == 0 & y_neighbour == 0 & z_neighbour == 0))

  count_active_neighbours <- all_points %>%
    merge(neighbours) %>%
    mutate(x_neighbouring_point = x + x_neighbour,
           y_neighbouring_point = y + y_neighbour,
           z_neighbouring_point = z + z_neighbour) %>%
    merge(active_cubes, by.x = c("x_neighbouring_point", "y_neighbouring_point", "z_neighbouring_point"), by.y = c("x_active", "y_active", "z_active")) %>%
    group_by(x,y,z) %>%
    summarise(active_neighbours = n())

  # keep active if a cube is active and exactly 2 or 3 of its neighbors are also active, the cube remains active
  keep_active <-
    count_active_neighbours %>%
    merge(active_cubes, by.x = c("x", "y", "z"), by.y = c("x_active", "y_active", "z_active")) %>%
    filter(active_neighbours ==2)

  # make/keep active if 3 of the neighbours are active
  make_active <-
    count_active_neighbours %>%
    filter(active_neighbours == 3)

  active_cubes <- bind_rows(keep_active, make_active) %>%
    select(x,y,z) %>%
    rename(x_active = x,
           y_active = y,
           z_active = z)
}

# part 2

# keep track of cubes that are active
active_cubes <- data.frame(x_active = ceiling(which(cubes_transformed == "#")/nrow(cubes_transformed))) %>%
  mutate(y_active = which(cubes_transformed == "#") - (x_active-1)*nrow(cubes_transformed),
         z_active = 0, w_active = 0)


for (reps in 1:6) { # number of times to iterate through
  # consider all of the neighbouring points
  all_points <- expand_grid(x = (min(active_cubes$x_active)-1):(max(active_cubes$x_active)+1),
                            y = (min(active_cubes$y_active)-1):(max(active_cubes$y_active)+1),
                            z = (min(active_cubes$z_active)-1):(max(active_cubes$z_active)+1),
                            w = (min(active_cubes$w_active)-1):(max(active_cubes$w_active)+1))

  # this is all 26 of the neighbours that will be checked for each point
  neighbours <- expand_grid(x_neighbour = -1:1, y_neighbour = -1:1, z_neighbour = -1:1, w_neighbour = -1:1) %>%
    filter(!(x_neighbour == 0 & y_neighbour == 0 & z_neighbour == 0 & w_neighbour == 0))

  count_active_neighbours <- all_points %>%
    merge(neighbours) %>%
    mutate(x_neighbouring_point = x + x_neighbour,
           y_neighbouring_point = y + y_neighbour,
           z_neighbouring_point = z + z_neighbour,
           w_neighbouring_point = w + w_neighbour) %>%
    merge(active_cubes, by.x = c("x_neighbouring_point", "y_neighbouring_point", "z_neighbouring_point", "w_neighbouring_point"), by.y = c("x_active", "y_active", "z_active", "w_active")) %>%
    group_by(x,y,z, w) %>%
    summarise(active_neighbours = n())

  # keep active if a cube is active and exactly 2 or 3 of its neighbors are also active, the cube remains active
  keep_active <-
    count_active_neighbours %>%
    merge(active_cubes, by.x = c("x", "y", "z", "w"), by.y = c("x_active", "y_active", "z_active", "w_active")) %>%
    filter(active_neighbours ==2)

  # make/keep active if 3 of the neighbours are active
  make_active <-
    count_active_neighbours %>%
    filter(active_neighbours == 3)

  active_cubes <- bind_rows(keep_active, make_active) %>%
    select(x,y,z, w) %>%
    rename(x_active = x,
           y_active = y,
           z_active = z,
           w_active = w)
}


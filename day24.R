
# Decription of script ---------------------------------------------------------
# Day 24 advent of code -- Lobby Layout
# ------------------------------------------------------------------------------

# set up -----------------------------------------------------------------------
library(tidyverse)
library(DescTools)

#%_% load data ------------------------------------------------------------------

tiles <- read.table("day24.txt")

# can split the tiles on e or w since all coordinates that they can be either end in e or w
tiles <- tiles %>%
  mutate(tile_number = row_number()) %>%
  separate_rows(V1, sep = "(?<=[ew])(?=.)")
# solve the puzzle -------------------------------------------------------------

coords <- data.frame(direction = c('ne', 'e', 'se', 'sw', 'w', 'nw'),
                     x = c(1, 1, 0, -1, -1, 0),
                     y = c(-1, 0, 1, 1, 0, -1))

tiles %>%
  merge(coords, by.x = "V1", by.y = "direction") %>%
  group_by(tile_number) %>%
  summarise(x = sum(x), y = sum(y)) %>%
  group_by(x,y) %>%
  summarise(count_tile = n()) %>%
  filter(count_tile %% 2 == 1) %>%
  nrow()

# part 2 (similar to day 17 Conway cubes)

black_hex <- tiles %>%
  merge(coords, by.x = "V1", by.y = "direction") %>%
  group_by(tile_number) %>%
  summarise(x = sum(x), y = sum(y)) %>%
  group_by(x,y) %>%
  summarise(count_tile = n()) %>%
  filter(count_tile %% 2 == 1) %>%
  select(black_x = x, black_y = y)

for (reps in 1:100) { # number of times to iterate through
  # consider all of the neighbouring points

  all_points <- expand_grid(black_hex, coords) %>%
    mutate(neighbour_x = black_x + x,
           neighbour_y = black_y + y)

  # convert current black tiles
  black_black_neighbours <-
    all_points %>%
    merge(black_hex, by.x = c("neighbour_x", "neighbour_y"), by.y = c("black_x", "black_y"))%>%
    group_by(black_x, black_y) %>%
    summarise(count = n())

  still_black <- black_hex %>%
    merge(black_black_neighbours, by = c("black_x", "black_y"), all.x = TRUE) %>%
    filter(!is.na(count)) %>%
    filter(count <= 2) %>%
    select(black_x, black_y)

  # convert white tiles

  convert_to_black <- all_points %>%
    group_by(neighbour_x, neighbour_y) %>%
    summarise(count = n()) %>%
    filter(count == 2) %>%
    filter(!paste(neighbour_x, neighbour_y) %in% paste(still_black$black_x, still_black$black_y)) %>%
    select(black_x = neighbour_x, black_y = neighbour_y)

  black_hex <- bind_rows(still_black, convert_to_black)

  print(nrow(black_hex))
}

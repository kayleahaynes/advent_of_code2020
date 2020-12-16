# Decription of script ---------------------------------------------------------
# Day 13 advent of code --Shuttle Search
# ------------------------------------------------------------------------------

# set up -----------------------------------------------------------------------
library(tidyverse)

# load data --------------------------------------------------------------------

bus_time <- read_table('day13.txt', col_names = FALSE)

time <- as.numeric(bus_time$X1[1])
buses <- data.frame(buses = as.numeric(str_split(bus_time$X1[2], pattern = ",")[[1]]))

# solve the puzzle -------------------------------------------------------------
# part 1

buses %>%
  filter(!is.na(buses)) %>%
  mutate(earliest_time = buses - (time %% buses)) %>%
  filter(earliest_time == min(earliest_time)) %>%
  summarise(answer = buses * earliest_time)

# part 2
# What is the earliest timestamp such that all of the listed bus IDs depart at offsets matching their positions in the list?
find_modulo_inverse <- function(n, m){
  which((((n - m * floor(n / m)) * seq(1:1000000)) %% m) == 1)[1]
}

# this is a bit of a hack to get the code to work
convert_remainder <- function(remainder, buses){
  if (remainder < 0){
    remainder <- ((remainder %% buses) + buses) %% buses
  }
  remainder
}

options(scipen = 999)

depart_times <- buses %>%
  mutate(after_timestamp = (row_number() - 1)) %>%
  mutate(remainder_org= buses -after_timestamp) %>%
  filter(!is.na(buses)) %>%
  mutate(remainder = unlist(map2(remainder_org, buses,convert_remainder))) %>%
  mutate(Ni = prod(buses)/buses,
         modulo_inverse = unlist(map2(Ni, buses, find_modulo_inverse)),
         modinv_check = unlist(map2(Ni, buses, modinv)),
         bNx = remainder * Ni * modulo_inverse)

### floating point issue doesn't give the right answer -- put this bit of calc into python
as.character(sum(depart_times$bNx) - floor(sum(depart_times$bNx) / prod(depart_times$buses)) * prod(depart_times$buses))


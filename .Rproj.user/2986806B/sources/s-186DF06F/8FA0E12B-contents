# Decription of script ---------------------------------------------------------
# Day 5 advent of code -- find seat number on plane
# ------------------------------------------------------------------------------

# set up -----------------------------------------------------------------------
library(tidyverse)

# load data --------------------------------------------------------------------
boarding_pass <-  read_delim(paste0(here::here(), "/day5.txt"), delim = "\n", col_names = FALSE)
boarding_pass <- boarding_pass %>%
  mutate(row = str_sub(X1, 1,7),
         col = str_sub(X1, 8,10))
# solve puzzle ------------------------------------------------------------------
# part 1
#
# Start by considering the whole range, rows 0 through 127.
# F means to take the lower half, keeping rows 0 through 63.
# B means to take the upper half, keeping rows 32 through 63.
# F means to take the lower half, keeping rows 32 through 47.
# B means to take the upper half, keeping rows 40 through 47.
# B keeps rows 44 through 47.
# F keeps rows 44 through 45.
# The final F keeps the lower of the two, row 44.
# The last three characters will be either L or R; these specify exactly one of the 8 columns of seats on the plane (numbered 0 through 7). The same process as above proceeds again, this time with only three steps. L means to keep the lower half, while R means to keep the upper half.
#
# For example, consider just the last 3 characters of FBFBBFFRLR:
#
#   Start by considering the whole range, columns 0 through 7.
# R means to take the upper half, keeping columns 4 through 7.
# L means to take the lower half, keeping columns 4 through 5.
# The final R keeps the upper of the two, column 5.

# function to get 1st or 2nd half of sequence

# half is either 1 for lower half or 2 for upper half
split_sequence <- function(sequence, half = 1){
  sequence[((half - 1) * (length(sequence)/2) + 1):(half * (length(sequence)/2))]
}

iterate_sequence <- function(sequence, binary_set){
  i <- 1
  while(i <= nchar(binary_set)){
    half <- ifelse(str_sub(binary_set,i,i) %in% c("F", "L") , 1, 2)
    sequence <- split_sequence(sequence, half)
    i <- i + 1
  }
  return(sequence)
}

boarding_pass %>%
  mutate(row_number = unlist(purrr::map(row, iterate_sequence, sequence = seq(0,127))),
         col_number = unlist(purrr::map(col, iterate_sequence, sequence = seq(0,7))),
         seat_number = row_number * 8 + col_number) %>%
  summarise(max(seat_number))

# part 2
# Your seat wasn't at the very front or back, though; the seats with IDs +1 and -1 from yours will be in your list.

boarding_pass %>%
  mutate(row_number = unlist(purrr::map(row, iterate_sequence, sequence = seq(0,127))),
         col_number = unlist(purrr::map(col, iterate_sequence, sequence = seq(0,7))),
         seat_number = row_number * 8 + col_number) %>%
  arrange(seat_number) %>%
  mutate(lag_seat = seat_number - lag(seat_number)) %>%
  top_n(1, lag_seat)


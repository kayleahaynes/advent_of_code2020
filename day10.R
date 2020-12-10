# Decription of script ---------------------------------------------------------
  # Day 8 advent of code --adaptor joltage
  # ------------------------------------------------------------------------------

# set up -----------------------------------------------------------------------
library(tidyverse)

# load data --------------------------------------------------------------------

joltage <- read_csv('day10.txt', col_names = FALSE)

# solve the puzzle
# part 1 - What is the number of 1-jolt differences multiplied by the number of 3-jolt differences?

sort_joltage <- sort(c(0,joltage$X1))
(sum((sort_joltage - lag(sort_joltage) == 1), na.rm = TRUE)) * (sum((sort_joltage - lag(sort_joltage) == 3), na.rm = TRUE) + 1)

# part 2 - find all combinations that can be used

diff_matrix <- matrix(nrow = length(sort_joltage)-1, ncol = length(sort_joltage)-1)
for (i in 1:(length(sort_joltage)-1)){
  for (j in (i + 1):length(sort_joltage)){
    diff_matrix[i,j-1] <- sort_joltage[j] -  sort_joltage[i]
  }
}

# only interested in 1s and 3s
diff_matrix[diff_matrix > 3] <- NA

row_sums <- rowSums(!is.na(diff_matrix))

t1 <- tibble(groups = cumsum(diff(c(0,row_sums)) > 0 | (diff(c(0,row_sums)) == 0 & lag(row_sums) != 3))) %>%
  group_by(groups) %>%
  mutate(count = n()) %>%
  distinct(groups, count) %>%
  mutate(triangle = ((count-1) * (count))/2 + 1)

prod(t1$triangle)

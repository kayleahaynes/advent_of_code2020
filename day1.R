# Decription of script ---------------------------------------------------------
# Day 1 advent of code -- adding and multiplying numbers.
# ------------------------------------------------------------------------------

# set up -----------------------------------------------------------------------
library(tidyverse)

# load data --------------------------------------------------------------------
spend_report <- read_csv(paste0(here::here(), "/day1.csv"), col_names = FALSE)
spend_report <- spend_report %>%
  rename(cost = X1)

# solve the puzzle -------------------------------------------------------------
# Part 1 (which 2 numbers sum to 2020 -- multiply these together)
for (i in 1:nrow(spend_report)){
  adds_to_2020 <- which((spend_report$cost[i] + spend_report$cost == 2020)) # add all the other costs to cost i and find which (if any) is equal to 2020
  if(length(adds_to_2020) > 0){ # if any do add to 2020 then multiply together and stop the loop
    print(spend_report$cost[i] * spend_report$cost[adds_to_2020])
    break
  }
}

# Part 2 (which 3 numbers sum to 2020 -- multiply these together)
stop <- FALSE
for (i in 1:nrow(spend_report)){
  for (j in 1:nrow(spend_report)){
    adds_to_2020 <- which((spend_report$cost[i] + spend_report$cost[j] + spend_report$cost == 2020)) # add all the other costs to cost i and find which (if any) is equal to 2020
    if(length(adds_to_2020) > 0){ # if any do add to 2020 then multiply together and stop the loop
      print(spend_report$cost[i] * spend_report$cost[j] * spend_report$cost[adds_to_2020])
      print(paste(i, j, adds_to_2020))
      stop = TRUE
      break
    }
    if(stop){
      break
    }
  }
}

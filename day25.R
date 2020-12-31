
# Decription of script ---------------------------------------------------------
# Day 25 advent of code -- Combo Breaker
# ------------------------------------------------------------------------------

# set up -----------------------------------------------------------------------
library(tidyverse)
library(DescTools)

#%_% load data ------------------------------------------------------------------

public_key1 <- 10943862
public_key2 <- 12721030

# solve the puzzle -------------------------------------------------------------
# part 1
find_loop_size <- function(public_key){
  value <- 1
  loop_size <- 0
  subject_number <- 7

  while(value != public_key){
    loop_size <- loop_size + 1
    # value equals self multiplied by subject number
    value <- value * subject_number
    # value equals remainder after dividing the value by 20201227
    value <- value %% 20201227
  }
  return(loop_size)
}

transform_key <- function(public_key, loop_size){
  value <- 1
  subject_number <- public_key

  for (i in 1:loop_size){
    value <- value * subject_number
    # value equals remainder after dividing the value by 20201227
    value <- value %% 20201227
  }
  return(value)
}

transform_key(public_key1, find_loop_size(public_key2))
transform_key(public_key2, find_loop_size(public_key1))

# Decription of script ---------------------------------------------------------
# Day 14 advent of code -- Rambunctious Recitation (memory game)
# ------------------------------------------------------------------------------

# set up -----------------------------------------------------------------------
library(tidyverse)

# load data --------------------------------------------------------------------

numbers <- c(16,11,15,0,1,7)

# solve the puzzle -------------------------------------------------------------
# part 1
memory <- data.frame(number = numbers, last_time_seen = 0,most_recent_time_seen = seq_along(numbers), number_times_seen = 1)

for (i in (length(numbers)+1):2020){
  if(memory[memory$most_recent_time_seen == i-1,]$number_times_seen == 1){
    if(nrow(memory[memory$number == 0,]) > 0){
      memory[memory$number == 0,]$last_time_seen <- memory[memory$number == 0,]$most_recent_time_seen
      memory[memory$number == 0,]$most_recent_time_seen <- i
      memory[memory$number == 0,]$number_times_seen <- memory[memory$number == 0,]$number_times_seen + 1
    }
    else {
      memory <- bind_rows(memory, data.frame(number = 0, last_time_seen = 0, most_recent_time_seen = i, number_times_seen = 1))
    }
  } else {
    new_number <- (i-1) - memory[memory$most_recent_time_seen == i-1,]$last_time_seen
    if(nrow(memory[memory$number == new_number,]) > 0){
      memory[memory$number == new_number,]$last_time_seen <- memory[memory$number == new_number,]$most_recent_time_seen
      memory[memory$number == new_number,]$most_recent_time_seen <- i
      memory[memory$number == new_number,]$number_times_seen <- memory[memory$number == new_number,]$number_times_seen + 1
    } else{
      memory <- bind_rows(memory, data.frame(number = new_number, last_time_seen = 0, most_recent_time_seen = i, number_times_seen = 1))
    }

  }
}

memory %>% filter(most_recent_time_seen == 2020)
# part 2 - need to change the code from above as this code took too long to run

memory <- rep(NA, 30000000)
memory[numbers + 1] <- c(1:length(numbers))

previous_times <- rep(NA, 30000000)
previous_times[numbers + 1] <- 0

ntimes <- rep(0, 30000000)
ntimes[numbers + 1] <- 1

last_number <- tail(numbers, 1)

for (i in (length(numbers)+1):30000000){

  if(ntimes[last_number + 1] == 1){
    next_number <- 0
  } else{
    next_number <-memory[last_number + 1] - previous_times[last_number + 1]
  }

  previous_times[next_number + 1] <- memory[next_number + 1]
  memory[next_number + 1] <- i
  ntimes[next_number + 1] <- ntimes[next_number + 1] + 1
  last_number <- next_number
}

which(memory == 30000000) - 1


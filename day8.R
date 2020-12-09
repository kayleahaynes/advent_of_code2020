# Decription of script ---------------------------------------------------------
# Day 8 advent of code --inflight computer
# ------------------------------------------------------------------------------

# set up -----------------------------------------------------------------------
library(tidyverse)

# load data --------------------------------------------------------------------

instructions <- read_delim(paste0(here::here(), "/day8.txt"), delim = " ", col_names = FALSE)
names(instructions) <- c("operation", "argument")

# solve the puzzle -------------------------------------------------------------
# part 1
i <- 1
k <- 0
acc <- 0
ran_instruction <- 1

while(k == 0){
  instruction <- instructions[i,]

  if(instruction$operation == "nop"){
    i <- i + 1
  } else if (instruction$operation == "acc"){
    i <- i + 1
    acc <- acc + instruction$argument
  }
  else if (instruction$operation == "jmp"){
    i <- i + instruction$argument
  }

  if (i %in% ran_instruction){
    k <- 1
  } else{
  ran_instruction <- c(ran_instruction, i)
  }
}

# part 2

test_loop <- function(change = 1, instructions){

  instructions_i <- instructions # don't want to change instructions
  instructions_i$operation[change] = ifelse (instructions_i$operation[change] == "jmp", "nop", "jmp")

  i <- 1
  k <- 0
  acc <- 0
  ran_instruction <- 1

  while(k == 0){
    # if the last row then terminate - if it's an acc then add on the number
    if (i == nrow(instructions_i)){
      if (instruction$operation == "acc"){
        acc <- acc + instruction$argument
      }
      return(acc)
    }
    else{
      instruction <- instructions_i[i,]

      if(instruction$operation == "nop"){
        i <- i + 1
      } else if (instruction$operation == "acc"){
        i <- i + 1
        acc <- acc + instruction$argument
      }
      else if (instruction$operation == "jmp"){
        i <- i + instruction$argument
      }

      if (i %in% ran_instruction){
        k <- 1
      } else{
        ran_instruction <- c(ran_instruction, i)
      }
    }
  }
    return(NA)
}

# loop over all of the nops and jmps to change. Change 1 at a time and see which one ends the instructions
for (i in which(instructions$operation %in% c("nop", "jmp"))){
  # if the program ends properly it will return the acc else NA
  if(!is.na(test_loop(i, instructions))){
    print(test_loop(i, instructions))
    break
  }
}


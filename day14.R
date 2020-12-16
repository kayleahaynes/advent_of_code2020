# Decription of script ---------------------------------------------------------
# Day 14 advent of code --Docking Data
# ------------------------------------------------------------------------------

# set up -----------------------------------------------------------------------
library(tidyverse)

# load data --------------------------------------------------------------------

program <- read_delim('day14.txt', col_names = FALSE, delim = " = ") %>%
  select(instruction = X1, value = X3)


# solve the puzzle -------------------------------------------------------------
# part 1

memory <- array()

for (i in 1:nrow(program)){
  if(program$instruction[i] == "mask"){
    mask <- program$value[i] # change the mask to be the new mask
  } else{
    bits <- c(rep(as.raw(0), 4), rev(intToBits(as.integer(program$value[i])))) # convert to bits
    for (j in 1:nchar(mask)){
      if (substr(mask,j,j) %in% c(1,0)){
        bits[j] <- as.raw(substr(mask,j,j))
      }
    }
    memory[parse_number(program$instruction[i])] <- sum(as.numeric(bits) * 2^seq(35,0))
  }
}

sum(memory, na.rm = TRUE)

# part 2

memory <- data.frame(location = NA, value = NA)

for (i in 1:nrow(program)){
  if(program$instruction[i] == "mask"){
    mask <- program$value[i] # change the mask to be the new mask
  } else{
    bits <- c(rep(as.raw(0), 4), rev(intToBits(as.integer(parse_number(program$instruction[i]))))) # convert to bits
    change_bit <- NULL
    for (j in 1:nchar(mask)){
      if (substr(mask,j,j) == 1){
        bits[j] <- as.raw(substr(mask,j,j))
      } else if (substr(mask,j,j) == "X"){
        change_bit <- c(change_bit,j)
      }
    }

   all_change_combo <- expand.grid(rep( list( 0:1 ), length(change_bit)))

   for (k in 1:nrow(all_change_combo)){
     bits[change_bit] <- as.raw(all_change_combo[k,])

     if(length(which(memory$location == (sum(as.numeric(bits) * 2^seq(35,0))))) == 0){
       memory <- rbind(memory, c((sum(as.numeric(bits) * 2^seq(35,0))), program$value[i]))
     } else {
       memory[which(memory$location == (sum(as.numeric(bits) * 2^seq(35,0)))),]$value <- program$value[i]
     }
   }
  }
}

sum(as.numeric(memory), na.rm = TRUE)

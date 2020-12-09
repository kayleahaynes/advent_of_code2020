# Decription of script ---------------------------------------------------------
# Day 8 advent of code --inflight computer
# ------------------------------------------------------------------------------

# set up -----------------------------------------------------------------------
library(tidyverse)

# load data --------------------------------------------------------------------

preamble <- read_csv('day9.txt', col_names = FALSE)

# solve the puzzle -------------------------------------------------------------
# part 1 - find the first number where the sum of any 2 numbers from the previous 25 numbers
# do not sum to this number

stop <- FALSE
i <- 1
m <- 25

while(stop == FALSE){

  check_numbers <- preamble[i:(i+(m-1)),] %>%
    mutate(need = preamble[i+m,]$X1 - preamble[i:(i+(m-1)),]$X1) %>%
    mutate(answer = need %in% preamble[i:(i+(m-1)),]$X1 & X1 != need) %>%
    filter(answer)

  if (nrow(check_numbers) == 0){
    print(preamble[i+m,]$X1)
    stop = TRUE
  }

  i <- i + 1
}

# part 2 - find a contiguous set of at least two numbers in your list which sum to the invalid number from step 1.

# only need to consider numbers that are less than the preamble
filter_numbers_less_than_target <- preamble %>%
  filter(X1 < preamble[i+m-1,]$X1)

for (j in 1:nrow(filter_numbers_less_than_target)){
  n <- 2
  stop = FALSE
  while(stop == FALSE){
    check_numbers <- sum(filter_numbers_less_than_target[j:(j+(n-1)),]$X1)

    # stop the while loop if the sum of the numbers is greater than or equal to the target number
    # or stop is n is too big
    if(check_numbers >=  preamble[(i-1)+m,]$X1 || (j + n) > nrow(filter_numbers_less_than_target)){
      stop = TRUE
    }
    n <- n + 1
  }
  # stop the for loop is the number has been found
  if (check_numbers ==  preamble[(i-1)+m,]$X1){
    break
  }
}

min(filter_numbers_less_than_target[j:(j+(n-2)),]$X1) + max(filter_numbers_less_than_target[j:(j+(n-2)),]$X1)






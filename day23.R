
# Decription of script ---------------------------------------------------------
# Day 23 advent of code -- Crab Cups
# ------------------------------------------------------------------------------

# set up -----------------------------------------------------------------------
library(tidyverse)
library(DescTools)

#%_% load data ------------------------------------------------------------------

input <- data.frame(number = 962713854)

input <- data.frame(numbers = input %>%
  separate(number, paste("col", 1:nchar(input$number[1])), sep = "(?<=.)",extra = "drop") %>%
  t()) %>%
  mutate(numbers = as.numeric(numbers))

# solve the puzzle -------------------------------------------------------------
# part 1

for (i in 1:100){
  current_cup_position<-1
  current_cup <- input$numbers[current_cup_position]

  three_cups <- input$numbers[(current_cup_position+1):(current_cup_position+3)]

  adjusted_cups <- input$numbers[-((current_cup_position+1):(current_cup_position+3))]

  destination_cup <- current_cup- 1

  destination_cup <- ifelse(all(destination_cup < input), max(input), destination_cup)

  while(destination_cup %in% three_cups){
    destination_cup <- destination_cup - 1
    if(all(destination_cup < input)){
      destination_cup <- max(input)
    }
  }

  if((which(adjusted_cups == destination_cup)) == length(adjusted_cups)){
    new_input <- c(adjusted_cups, three_cups)
  } else{
    new_input <- c(adjusted_cups[1:which(adjusted_cups == destination_cup)], three_cups,
                   adjusted_cups[(which(adjusted_cups == destination_cup)+1):length(adjusted_cups)])
  }

  # wrap around
  input <- data.frame(numbers = c(new_input, new_input[1])[-1])
}

c(input[which(input == 1):nrow(input),], input[1:(which(input==1)-1),])

# part 2
## attempt 1 -- naively thought this would work but is far too slow to run
input <- data.frame(number = 962713854)

input <- data.frame(numbers = input %>%
                      separate(number, paste("col", 1:nchar(input$number[1])), sep = "(?<=.)",extra = "drop") %>%
                      t()) %>%
  mutate(numbers = as.numeric(numbers)) %>%
  mutate(position = row_number())

# add the last row to help with tracking
#
input <- bind_rows(input, data.frame(numbers = 1000000, position = 1000000))

for (i in 1:10000000){

  if(i %% 1000 == 0) {print(i)}
  current_cup_position<-1

  # if position 1 is missing
  if(length(input$numbers[input$position == 1]) == 0){
    current_cup <- max(input$numbers[input$numbers != 1000000]) + 1
  } else {
    current_cup <- input$numbers[current_cup_position]
  }

  # if positions 2,3,4 are missing

  if(any(!c(2,3,4) %in% input$position)){
    three_cups <- (current_cup + 1):(current_cup + 3)
    adjusted_cups <- input
  } else{
    three_cups <- input$numbers[(current_cup_position+1):(current_cup_position+3)]
    adjusted_cups <- input[-((current_cup_position+1):(current_cup_position+3)),]
  }

  adjusted_cups <- adjusted_cups %>%
    mutate(position = ifelse(position == 1, position, position - 3))

  destination_cup <- current_cup- 1

  destination_cup <- ifelse(all(destination_cup < input),  max(adjusted_cups$numbers), destination_cup)

  while(destination_cup %in% three_cups){
    destination_cup <- destination_cup - 1
    if(all(destination_cup < input)){
      destination_cup <- max(adjusted_cups$numbers)
    }
  }

  # adjusted_cups <- bind_rows(adjusted_cups,
  #                            data.frame(numbers = (max(adjusted_cups)+1):(max(adjusted_cups)+3),
  #                                       position = 7:9))

  new_input <- adjusted_cups %>%
    mutate(position = ifelse(position > adjusted_cups$position[adjusted_cups$numbers == destination_cup],
                             position + 3, position))

  new_input <- bind_rows(new_input,
                         data.frame(numbers = three_cups, position = (adjusted_cups$position[adjusted_cups$numbers == destination_cup] + 1):(adjusted_cups$position[adjusted_cups$numbers == destination_cup] + 3)))

  # wrap around
  input <- new_input %>%
    mutate(position = ifelse(position == 1, 1000000, position - 1)) %>%
    arrange(position)
}


### attempt 2 -- required hint from https://selbydavid.com/2020/12/06/advent-2020/#day23 to help with the speed

input <-c(9,6,2,7,1,3,8,5,4, 10:1e6)

# Linked list where each index is the label of the preceding cup.
lnklst <- c(input[-1], input[1])[order(input)]

current_cup <- input[1]

for (i in 1:1e7){
  three_cups1 <- lnklst[current_cup]
  three_cups2 <- lnklst[three_cups1]
  three_cups3 <- lnklst[three_cups2]


  # Choose destination.
  dest <- 1 + (current_cup - 1:4 - 1) %% ncups # there must be atleast 1 cup from 1:4 less that doesn't appear in the 3 cups
  destination_cup<- dest[which.min(dest %in% c(three_cups1, three_cups2, three_cups3))]

  # remove and slot in the 3 cups
  lnklst[current_cup] <- lnklst[three_cups3] # the number preceding the current cup is the number preceding the 3rd cup
  lnklst[three_cups3] <- lnklst[destination_cup] # the number preceding the 3rd cup is the current number preceding the destination cup
  lnklst[destination_cup] <- three_cups1 # the number preceding the destination cup is the first of thr 3 cups

  # note the number preceding cup1 and cup2 doesn't change

  current_cup <- lnklst[current_cup] #  move clockwise by 1
}

lnklst[1] * lnklst[lnklst[1]]


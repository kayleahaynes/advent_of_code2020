# Decription of script ---------------------------------------------------------
# Day 16 advent of code -- Ticket Translation
# ------------------------------------------------------------------------------

# set up -----------------------------------------------------------------------
library(tidyverse)

# load data --------------------------------------------------------------------

rules <- read_csv('day16_a.txt', col_names = FALSE)
nearby_tickets <- read_csv('day16_c.txt', col_names = FALSE)
my_ticket <- read_csv('day16_b.txt', col_names = FALSE)

# solve the puzzle
# part 1

find_valid_numbers <- rules %>%
  extract(X1, c("info", "min1", "max1", "min2", "max2"), '(.*): (\\d+)-(\\d+) or (\\d+)-(\\d+)') %>%
  mutate(valid1 = map2(min1, max1, seq),
         valid2 = map2(min2, max2, seq))

valid_numbers <- unique(c(unlist(find_valid_numbers$valid1), unlist(find_valid_numbers$valid2)))

invalid <- sum(as.matrix(nearby_tickets)[which(!as.matrix(nearby_tickets) %in% valid_numbers)])

# part 2
# discard rows with any invalid
valid_tickets <- nearby_tickets[rowSums(matrix(as.matrix(nearby_tickets) %in% valid_numbers, ncol = ncol(nearby_tickets), nrow = nrow(nearby_tickets))) == 20,]

ticket_parts <- data.frame()

for (i in 1:ncol(valid_tickets)){
  for (j in 1:nrow(find_valid_numbers)){
    valid_numbers <- unique(c(unlist(find_valid_numbers$valid1[[j]]), unlist(find_valid_numbers$valid2[[j]])))
    ticket_parts[i,j] = sum(!valid_tickets[,i] %>% pull() %in% valid_numbers) == 0
  }
}

# count the number of options we have for each field

names(ticket_parts) = find_valid_numbers$info
ticket_parts_decided <- rep(NA, nrow(ticket_parts))

while(!is.null(nrow(ticket_parts))){

  names(ticket_parts)[which(as.numeric(ticket_parts[which(rowSums(ticket_parts) == 1),]) == 1)]
    ticket_parts_decided[as.numeric(names(which(rowSums(ticket_parts) == 1)))] <- names(ticket_parts)[which(as.numeric(ticket_parts[which(rowSums(ticket_parts) == 1),]) == 1)]
    ticket_parts <- ticket_parts[-which(rowSums(ticket_parts) == 1),-which(as.numeric(ticket_parts[which(rowSums(ticket_parts) == 1),]) == 1)]
}

# fill in the last part

ticket_parts_decided[is.na(ticket_parts_decided)] <- setdiff(find_valid_numbers$info, ticket_parts_decided )

prod(my_ticket[which(ticket_parts_decided %like% "departure")])

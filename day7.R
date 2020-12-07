# Decription of script ---------------------------------------------------------
# Day 7 advent of code - check bags-
# ------------------------------------------------------------------------------

# set up -----------------------------------------------------------------------
library(tidyverse)

# load data --------------------------------------------------------------------
bags <-  data.frame(combination = read_lines(paste0(here::here(), "/day7.txt")))# remove the trailing new line

# solve the puzzle--------------------------------------------------------------
# part 1 - how many combinations of bags can contain shiny gold bags?

bag_combos <- bags %>%
  mutate(combination = str_replace_all(combination, c("bags|bag|contain|,|\\."), "")) %>%
  separate_rows(combination, sep = "  ") %>%
  mutate(count = as.numeric(ifelse(combination == " no other ", 0, parse_number(combination)))) %>%
  mutate(contains_bag_colour = str_trim(str_replace_all(combination, as.character(count), ""))) %>%
  mutate(original_bag_colour = ifelse(is.na(count), combination, NA)) %>%
  tidyr::fill(original_bag_colour, .direction = "down") %>%
  filter(!is.na(count)) %>%
  select(original_bag_colour, contains_bag_colour, count) %>%
  arrange(contains_bag_colour)

# find the combinations of bags that explicitly say they have shiny gold bags
combinations_to_check<- bag_combos$original_bag_colour[which(bag_combos$contains_bag_colour == "shiny gold")]
all_bag_combinations <- combinations_to_check

while(length(combinations_to_check) > 0){
  new_combinations <- bag_combos$original_bag_colour[which(bag_combos$contains_bag_colour == combinations_to_check[1])]
  combinations_to_check <- combinations_to_check[-1] # remove the current checked bag
  if(length(new_combinations) > 0){
    combinations_to_check <- unique(c(combinations_to_check, new_combinations))
    all_bag_combinations <- unique(c(all_bag_combinations, new_combinations))
  }
}

length(all_bag_combinations)

# part 2 - How many individual bags are required inside your single shiny gold bag?
next_bags <- bag_combos %>%
  filter(original_bag_colour == "shiny gold") # count number of bags in shiny gold bag

bag_count <- sum(next_bags$count) # keep a track of the count of bags

while(sum(next_bags$count) > 0){  # recursive until all bags contain no more bags
  next_bags <- bag_combos %>%
    filter(original_bag_colour %in% next_bags$contains_bag_colour)%>%  # find the next bags
    merge(next_bags, by.x = "original_bag_colour", by.y = "contains_bag_colour") %>% # merge with the count of bags
    mutate(total_count = count.x * count.y) %>% # get the total number of bags
    select(original_bag_colour, contains_bag_colour, count = total_count) # select combinations and counts

  bag_count = bag_count + sum(next_bags$count)
}




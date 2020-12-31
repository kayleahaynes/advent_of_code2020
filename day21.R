# Decription of script ---------------------------------------------------------
# Day 21 advent of code -- Allergen Assessment
# ------------------------------------------------------------------------------

# set up -----------------------------------------------------------------------
library(tidyverse)
library(DescTools)

#%_% load data --------------------------------------------------------------------

ingredients <- data.frame(food_list = readLines('day21.txt')) %>%
  mutate(food_list = gsub("contains", "", food_list)) %>%
  extract(food_list, c("ingredients", "allergens"), "(^[^\\(]+) \\((.*?)\\)") %>%
  mutate(line = row_number())

# solve the puzzle -------------------------------------------------------------
split_ingredients <- ingredients %>%
  separate_rows(ingredients, sep = " ") %>%
  separate_rows(allergens, sep = ", ") %>%
  mutate(allergens = gsub(" ", "", allergens)) %>%
  arrange(allergens)

save_allergens <- NULL
split_ingredients_temp <- split_ingredients
k <- 1

while(nrow(split_ingredients_temp) > 0){
  allergen_i <- unique(split_ingredients_temp$allergens)[k]

  split_ingredients_temp <- split_ingredients_temp %>%
    group_by(ingredients, allergens) %>%
    mutate(ingredient_allergen_count = n()) %>%
    group_by(allergens) %>%
    mutate(allergen_count = length(unique(line))) %>%
    distinct()

  check_allergen <- split_ingredients_temp %>%
    filter(allergens == allergen_i) %>%
    filter(ingredient_allergen_count == allergen_count) %>%
    select(ingredients, allergens) %>%
    distinct()

  if(nrow(check_allergen) == 1){
    save_allergens <- bind_rows(save_allergens, data.frame(check_allergen$ingredients, check_allergen$allergens))
    split_ingredients_temp <- split_ingredients_temp %>%
      filter(allergens != allergen_i,
             ingredients != check_allergen$ingredients)
    k <- 1
  } else{
    k <- k + 1
  }
}

# find ingredients with no allergens

split_ingredients %>%
  filter(!ingredients %in% save_allergens$check_allergen.ingredients) %>%
  select(ingredients, line) %>%
  distinct() %>%
  nrow()

# part 2

save_allergens %>%
  arrange(check_allergen.allergens) %>%
  select(check_allergen.ingredients) %>%
  pull() %>%
  paste(collapse = ",")

# Decription of script ---------------------------------------------------------
# Day 2 advent of code -- find valid passwords based on different policies.
# This code used regex and text matching.
# ------------------------------------------------------------------------------

# set up -----------------------------------------------------------------------
library(tidyverse)

# load data --------------------------------------------------------------------
# Each line gives the password policy and then the password. The password policy indicates the lowest and highest number of times a given letter must appear for the password to be valid. For example, 1-3 a means that the password must contain a at least 1 time and at most 3 times.
passwords <- read_csv(paste0(here::here(), "/day2.csv"), col_names = FALSE)
passwords <-passwords %>%
  rename(passwords_and_policy = X1)

# solve the puzzle -------------------------------------------------------------
# Part 1: How many passwords are valid according to their policies (letter must appear greater than the minimum occurance and less than the maximum occurance).
# use string matching to extract the different parts of the files related to the password and the policies. Count how many times a letter appears in a password and filter out invalid passwords
extract_string_match <- function(string, pattern, element = 1){
  str_match_all(string, pattern = pattern)[[1]][element]
}

count_occurance <- function(password, letter){
  length(unlist(str_match_all(password, pattern = letter)))
}

valid_passwords <- passwords %>%
  mutate(min_occurance = as.numeric(unlist(purrr::map(passwords_and_policy, extract_string_match, pattern = "[0-9]+", element = 1))),
         max_occurance = as.numeric(unlist(purrr::map(passwords_and_policy, extract_string_match, pattern = "[0-9]+", element = 2))),
         letter = unlist(purrr::map(passwords_and_policy, extract_string_match, pattern = "[a-z]")),
         password = unlist(purrr::map(passwords_and_policy, extract_string_match, pattern = "[a-z][^:]+\\s*"))
         ) %>%
  mutate(count_occurance = unlist(purrr::map2(password, letter, count_occurance))) %>%
  filter(count_occurance >= min_occurance & count_occurance <= max_occurance)

number_of_valid_passwords <- nrow(valid_passwords)

# Part 2 How many passwords are valid according to their policies where a letter must appear in either one of the positions (but not both).

find_position_of_letter <- function(password, letter){
  paste0(",", paste(str_locate_all(password, pattern = letter)[[1]][,1], collapse = ","), ",")
}

valid_passwords <- passwords %>%
  mutate(position1 = as.numeric(unlist(purrr::map(passwords_and_policy, extract_string_match, pattern = "[0-9]+", element = 1))),
         position2 = as.numeric(unlist(purrr::map(passwords_and_policy, extract_string_match, pattern = "[0-9]+", element = 2))),
         letter = unlist(purrr::map(passwords_and_policy, extract_string_match, pattern = "[a-z]")),
         password = unlist(purrr::map(passwords_and_policy, extract_string_match, pattern = "[a-z][^:]+\\s*"))
  ) %>%
  mutate(positions = unlist(purrr::map2(password, letter, find_position_of_letter))) %>%
  mutate(letter_in_position1 = unlist(purrr::map2(positions, paste0(",", as.character(position1), ","), count_occurance)),
         letter_in_position2 = unlist(purrr::map2(positions, paste0(",", as.character(position2), ","), count_occurance))) %>%
  filter(letter_in_position1+letter_in_position2 == 1)

number_of_valid_passwords <- nrow(valid_passwords)

### This below code is an alternative solution copied from @drob which is really neat ------------
passwords_t1 <- passwords %>%
  extract(passwords_and_policy, c("min", "max", "letter", "password"),
          "(\\d+)-(\\d+) (.): *(.*)")

passwords_t1 %>%
  mutate(count = map2_int(password, letter, str_count)) %>%
  filter(count >= min, count <= max)

passwords_t1 %>%
  mutate(count = (str_sub(password, min, min) == letter) +
           (str_sub(password, max, max) == letter)) %>%
  filter(count == 1)

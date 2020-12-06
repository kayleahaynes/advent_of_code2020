# Decription of script ---------------------------------------------------------
# Day 4 advent of code -- checking passport validations
# ------------------------------------------------------------------------------

# set up -----------------------------------------------------------------------
library(tidyverse)

# load data --------------------------------------------------------------------
passports <- data.frame(entries = read_file(paste0(here::here(), "/day4.txt")))

# solve the puzzle -------------------------------------------------------------
# Part 1 Passports should all have 8 fields unless cid is missing in which case count it as valid
passports %>%
  separate_rows(entries, sep = "\n\n") %>% # a blank line separates passports
  mutate(passport_id = paste0("Passport", 1:n())) %>% # keep track of passports by given them an id
  separate_rows(entries, sep = " ") %>%  # separate key:values from space
  separate_rows(entries, sep = "\n") %>% # separate key:values from new line
  filter(entries != "") %>%  # filter out blanks
  separate(entries, into = c("key", "value"), sep = ":") %>%  # separate into key values
  group_by(passport_id) %>% # group by passort id
  mutate(number_entries = n()) %>%  # count number of entries on each passport
  mutate(cid_exists = any(key == "cid")) %>%
  filter(number_entries == 8 | (number_entries == 7 & cid_exists == FALSE)) %>% # find passports which satisfy the condition
  select(passport_id) %>% # select passports
  ungroup() %>%
  distinct() %>% # get the distinct passwords
  count() # count how many there are

# Part 2 -- add more validation to the rules
# byr (Birth Year) - four digits; at least 1920 and at most 2002.
# iyr (Issue Year) - four digits; at least 2010 and at most 2020.
# eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
# hgt (Height) - a number followed by either cm or in:
#   If cm, the number must be at least 150 and at most 193.
# If in, the number must be at least 59 and at most 76.
# hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
# ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
# pid (Passport ID) - a nine-digit number, including leading zeroes.
# cid (Country ID) - ignored, missing or not.

passports %>%
  separate_rows(entries, sep = "\n\n") %>% # a blank line separates passports
  mutate(passport_id = paste0("Passport", 1:n())) %>% # keep track of passports by given them an id
  separate_rows(entries, sep = " ") %>%  # separate key:values from space
  separate_rows(entries, sep = "\n") %>% # separate key:values from new line
  filter(entries != "") %>%  # filter out blanks
  separate(entries, into = c("key", "value"), sep = ":") %>%  # separate into key values
  group_by(passport_id) %>% # group by passort id
  mutate(number_entries = n()) %>%  # count number of entries on each passport
  mutate(cid_exists = any(key == "cid")) %>%
  filter(number_entries == 8 | (number_entries == 7 & cid_exists == FALSE)) %>% # find passports which satisfy the condition
  mutate(height_units = ifelse(key == "hgt", ifelse(nchar(value) == 5, str_sub(value, 4,5), str_sub(value, 3,4)), "NA")) %>% # this checks the height is numeric and then character
  mutate(validation = case_when(key == "byr" ~ ifelse(nchar(value) == 4 & as.numeric(value) >= 1920 & as.numeric(value) <= 2002, TRUE, FALSE),
                                key == "iyr" ~ ifelse(nchar(value) == 4 & as.numeric(value) >= 2010 & as.numeric(value) <= 2020, TRUE, FALSE),
                                key == "eyr" ~ ifelse(nchar(value) == 4 & as.numeric(value) >= 2020 & as.numeric(value) <= 2030, TRUE, FALSE),
                                key == "hgt" ~ ifelse((height_units == "cm" & readr::parse_number(value) >= 150 & readr::parse_number(value) <= 193) |
                                                      (height_units == "in" & readr::parse_number(value) >= 59 & readr::parse_number(value) <= 76), TRUE, FALSE),
                                key == "hcl" ~ ifelse(str_sub(value, 1, 1) == "#" & nchar(str_extract_all(value, '[0-9, a-f]+')) == 6, TRUE, FALSE),
                                key == "ecl" ~ ifelse(nchar(value) == 3 & (value %in% c("amb", "blu", "brn", "gry", "grn", "hzl", "oth")), TRUE, FALSE),
                                key == "pid" ~ ifelse(nchar(str_extract_all(value, '[0-9]+')) == 9, TRUE, FALSE),
                                key == "cid" ~ TRUE)) %>%
  mutate(count_valid = sum(validation)) %>% # count the number of valid entries per passport
  filter(number_entries == count_valid) %>% # check all of the entries are valid
  select(passport_id) %>% # select passports
  ungroup() %>%
  distinct() %>% # get the distinct passwords
  count() # count how many there are

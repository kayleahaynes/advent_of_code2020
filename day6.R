# Decription of script ---------------------------------------------------------
# Day 6 advent of code - customs forma questionaire
# ------------------------------------------------------------------------------

# set up -----------------------------------------------------------------------
library(tidyverse)

# load data --------------------------------------------------------------------
customs_form <-  data.frame(answers = str_sub(read_file(paste0(here::here(), "/day6.txt")), end = -2)) # remove the trailing new line

# solve the puzzle--------------------------------------------------------------
# part 1 - how many unique questions did each group answer year? (a letter means they answered yes)
count_unique <- function(answer){
  length(unique(str_split(answer, "")[[1]]))
}

count_all_answered <- function(answer){
  # the first string in table is \n which represents a new person (this plus 1 is the number of people in a group)
  count_questions <- table(str_split(answer, "")[[1]])
  group_size = ifelse(!any(names(count_questions) == "\n"), 1, count_questions[names(count_questions) == "\n"] + 1)
  sum(count_questions/group_size == 1)
}

customs_form %>%
  separate_rows(answers, sep = "\n\n") %>%
  mutate(answers = str_replace_all(answers, "\n", "")) %>%
  mutate(unique_questions = unlist(purrr::map(answers, count_unique))) %>%
  summarise(sum(unique_questions))

# part 2
# You don't need to identify the questions to which anyone answered "yes"; you need to identify the questions to which everyone answered "yes"!

customs_form %>%
  separate_rows(answers, sep = "\n\n") %>%
  mutate(questions_all_answered = unlist(purrr::map(answers, count_all_answered))) %>%
  summarise(sum(questions_all_answered))

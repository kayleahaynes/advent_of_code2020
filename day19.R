# Decription of script ---------------------------------------------------------
# Day 19 advent of code -- Monster Messages
# ------------------------------------------------------------------------------

# set up -----------------------------------------------------------------------
library(tidyverse)

#%_% load data --------------------------------------------------------------------

rules <- read_delim('day19_a.txt', delim = ":", col_names = FALSE)
entries <- read_csv('day19_b.txt', col_names = FALSE)

names(rules) <- c("rule_number", "rule")

rules$rule <- str_trim(rules$rule)

rules <- rules %>%
  mutate(rule_number = as.numeric(rule_number)) %>%
  arrange(rule_number)
# solve the puzzle ------------------------------------------------------------
# part 1

format_regex <- function(rule_i){

  if (is.na(rule_i)){
    return(NA)
  }
  else{
    rule_i <- which(rules$rule_number == rule_i)
    if (rules[rule_i, ]$rule %in% c("a", "b")){
      return(rules[rule_i, ]$rule)
    } else {
      # find the right format for the rules
      extract_rules <- rules[rule_i,] %>%
        extract(rule, c("rule1", "rule2", "rule3"),
                '(\\d+) (\\d+) (\\d+)')

      if (is.na(sum(as.numeric(extract_rules)))){
        extract_rules <- rules[rule_i,] %>%
          extract(rule, c("rule1", "rule2", "rule3", "rule4"),
                  '(\\d+) (\\d+) \\| (\\d+) (\\d+)')
      }

      if (is.na(sum(as.numeric(extract_rules)))){
        extract_rules <- rules[rule_i,] %>%
          extract(rule, c("rule1", "rule2"),
                  '(\\d+) (\\d+)')
      }

      if (is.na(sum(as.numeric(extract_rules)))){
        extract_rules <- rules[rule_i,] %>%
          extract(rule, c("rule1", "rule3"),
                  '(\\d+) \\| (\\d+)')
      }

      if (is.na(sum(as.numeric(extract_rules)))){
        extract_rules <- rules[rule_i,] %>%
          extract(rule, c("rule1"),
                  '(\\d+)')
      }

      if(ncol(extract_rules) == 3 & length(extract_rules$rule2) > 0){
        extract_rules <- extract_rules %>%
          mutate(rule3 = NA, rule4 = NA)
      } else if(ncol(extract_rules) == 3 & length(extract_rules$rule2) == 0){
        extract_rules <- extract_rules %>%
          mutate(rule2 = NA, rule4 = NA)
      } else if(ncol(extract_rules) == 2){
        extract_rules <- extract_rules %>%
          mutate(rule2 = NA, rule3 = NA, rule4 = NA)
      } else if(ncol(extract_rules) == 4){
        extract_rules <- extract_rules %>%
          mutate(rule4 = NA)
      }
    }

    if(!is.na(extract_rules$rule3) & is.na(extract_rules$rule4) & !is.na(extract_rules$rule2)){
      return(paste0("(", format_regex(as.numeric(extract_rules$rule1)), ")(",
                    format_regex(as.numeric(extract_rules$rule2)), ")(",
                    format_regex(as.numeric(extract_rules$rule3)), ")"))
    } else{

      left_rule <- c(format_regex(as.numeric(extract_rules$rule1)), format_regex(as.numeric(extract_rules$rule2)))
      left_rule <- paste0(left_rule[!is.na(left_rule)], collapse = "")

      right_rule <- c(format_regex(as.numeric(extract_rules$rule3)), format_regex(as.numeric(extract_rules$rule4)))
      right_rule <- paste0(right_rule[!is.na(right_rule)], collapse = "")

      return(paste0("(", left_rule, "|", right_rule, ")"))
    }
  }
}


all_rules <- paste0("^", format_regex(0), "$")
all_rules <- gsub("|)", ")", all_rules, fixed = TRUE)

sum(str_detect(entries$X1, all_rules))

# part 2

rules <- read_delim('day19_c.txt', delim = ":", col_names = FALSE)

names(rules) <- c("rule_number", "rule")

rules$rule <- str_trim(rules$rule)

rules <- rules %>%
  mutate(rule_number = as.numeric(rule_number)) %>%
  arrange(rule_number)

format_regex <- function(rule_i){

  if (is.na(rule_i)){
    return(NA)
  } else if (!rule_i %in% c(8,11)){ # treat 8 and 11 differently
    rule_i <- which(rules$rule_number == rule_i)
    if (rules[rule_i, ]$rule %in% c("a", "b")){
      return(rules[rule_i, ]$rule)
    } else {
      # find the right format for the rules
      extract_rules <- rules[rule_i,] %>%
        extract(rule, c("rule1", "rule2", "rule3"),
                '(\\d+) (\\d+) (\\d+)')

      if (is.na(sum(as.numeric(extract_rules)))){
        extract_rules <- rules[rule_i,] %>%
          extract(rule, c("rule1", "rule2", "rule3", "rule4"),
                  '(\\d+) (\\d+) \\| (\\d+) (\\d+)')
      }

      if (is.na(sum(as.numeric(extract_rules)))){
        extract_rules <- rules[rule_i,] %>%
          extract(rule, c("rule1", "rule2"),
                  '(\\d+) (\\d+)')
      }

      if (is.na(sum(as.numeric(extract_rules)))){
        extract_rules <- rules[rule_i,] %>%
          extract(rule, c("rule1", "rule3"),
                  '(\\d+) \\| (\\d+)')
      }

      if (is.na(sum(as.numeric(extract_rules)))){
        extract_rules <- rules[rule_i,] %>%
          extract(rule, c("rule1"),
                  '(\\d+)')
      }

      if(ncol(extract_rules) == 3 & length(extract_rules$rule2) > 0){
        extract_rules <- extract_rules %>%
          mutate(rule3 = NA, rule4 = NA)
      } else if(ncol(extract_rules) == 3 & length(extract_rules$rule2) == 0){
        extract_rules <- extract_rules %>%
          mutate(rule2 = NA, rule4 = NA)
      } else if(ncol(extract_rules) == 2){
        extract_rules <- extract_rules %>%
          mutate(rule2 = NA, rule3 = NA, rule4 = NA)
      } else if(ncol(extract_rules) == 4){
        extract_rules <- extract_rules %>%
          mutate(rule4 = NA)
      }
    }

    if(!is.na(extract_rules$rule3) & is.na(extract_rules$rule4) & !is.na(extract_rules$rule2)){
      return(paste0("(", format_regex(as.numeric(extract_rules$rule1)), ")(",
                    format_regex(as.numeric(extract_rules$rule2)), ")(",
                    format_regex(as.numeric(extract_rules$rule3)), ")"))
    } else{

      left_rule <- c(format_regex(as.numeric(extract_rules$rule1)), format_regex(as.numeric(extract_rules$rule2)))
      left_rule <- paste0(left_rule[!is.na(left_rule)], collapse = "")

      right_rule <- c(format_regex(as.numeric(extract_rules$rule3)), format_regex(as.numeric(extract_rules$rule4)))
      right_rule <- paste0(right_rule[!is.na(right_rule)], collapse = "")

      return(paste0("(", left_rule, "|", right_rule, ")"))
    }
  } else {
    rule_i <- which(rules$rule_number == rule_i)

    extract_rules <- rules[rule_i,] %>%
      extract(rule, c("rule1", "rule2", "rule3", "rule4", "rule5"),
              '(\\d+) (\\d+) \\| (\\d+) (\\d+) (\\d+)')

    if (!is.na(sum(as.numeric(extract_rules)))){

      return(paste0("(", format_regex(as.numeric(extract_rules$rule1)), "){", 1:10, "}(",  format_regex(as.numeric(extract_rules$rule2)), "){", 1:10, "}") %>%
        paste(collapse = "|") %>%
        paste0("(", ., ")"))
    } else {

    extract_rules <- rules[rule_i,] %>%
      extract(rule, c("rule1", "rule2", "rule3"),
              '(\\d+) \\| (\\d+) (\\d+)')

    return(paste0("(", format_regex(as.numeric(extract_rules$rule1)), "+)"))
    }
  }
}


all_rules <- paste0("^", format_regex(0), "$")
all_rules <- gsub("|)", ")", all_rules, fixed = TRUE)

sum(str_detect(entries$X1, all_rules))

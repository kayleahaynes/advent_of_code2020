# Decription of script ---------------------------------------------------------
# Day 18 advent of code -- Operations order
# ------------------------------------------------------------------------------

# set up -----------------------------------------------------------------------
library(tidyverse)

operations <- readLines('day18.txt')

# solve the puzzle -------------------------------------------------------------
# part 1 - replace + and * with infix functions %*% and %+% (http://adv-r.had.co.nz/Functions.html) note R’s default precedence rules mean that infix operators are composed from left to right

"%+%" <- function(x,y){x + y}
"%*%" <- function(x,y){x * y}

operations <- gsub("+", "%+%", operations, fixed = TRUE)
operations <- gsub("*", "%*%", operations, fixed = TRUE)

evaluation <- function(X){
  eval(parse(text = X))
}

sum(map_dbl(operations, evaluation))

# part 2

operations <- readLines('day18.txt')

"%+%" <- function(x,y){x + y}
operations <- gsub("+", "%+%", operations, fixed = TRUE)

sum(map_dbl(operations, evaluation))



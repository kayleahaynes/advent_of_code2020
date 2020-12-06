# Decription of script ---------------------------------------------------------
# Day 3 advent of code -- Toboggan Trajectory
#
# ------------------------------------------------------------------------------

# set up -----------------------------------------------------------------------
library(tidyverse)

# load data --------------------------------------------------------------------

geology <- read_table(paste0(here::here(), "/day3.txt"), col_names = FALSE)

geology_transformed <- geology %>%
  separate(X1, paste("col", 1:nchar(geology$X1[1])), sep = "(?<=.)", extra = "drop")

# solve the puzzle -------------------------------------------------------------
# Part 1:- count the number of trees encountered if you take the path
# 3 across and 1 down

nrows <- nrow(geology_transformed)
ncols <- ncol(geology_transformed)
route_col <- 3*(1:nrows)
relative_postition <- route_col%%ncols + 1

tree_count <- 0
for (i in 2:nrows){
  tree_count <- tree_count + (geology_transformed[i, relative_postition[i-1]] == "#")
}

# Part 2
# now try different combinations of slopes
# Right 1, down 1.
# Right 3, down 1. (This is the slope you already checked.)
# Right 5, down 1.
# Right 7, down 1.
# Right 1, down 2.

# create a function to work out the position of the slope
slope_function <- function(right, down){
  route_col <- right*(1:round(nrows/down))
  relative_postition <- route_col%%ncols + 1
  tree_count <- 0
  for (i in 1:(round(nrows/down)-1)){
    tree_count <- tree_count + (geology_transformed[1 + (down * i), relative_postition[i]] == "#")
  }
  return(tree_count)
}

# multiple the slopes together
slope_function(1,1) * slope_function(3,1) * slope_function(5,1) * slope_function(7,1) * slope_function(1,2)

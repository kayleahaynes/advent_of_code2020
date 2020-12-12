# Day 11 advent of code -- seating
#
# ------------------------------------------------------------------------------

# set up -----------------------------------------------------------------------
library(tidyverse)

# load data --------------------------------------------------------------------

seats <- read_table(paste0(here::here(), "/day11.txt"), col_names = FALSE)

seats_transformed <- seats %>%
  separate(X1, paste("col", 1:nchar(seats$X1[1])), sep = "(?<=.)",extra = "drop") %>%
  as.matrix()

seats_transformed[seats_transformed == "L"] <- 0
seats_transformed[seats_transformed == "."] <- NA

seats_transformed <- matrix(as.numeric(seats_transformed), nrow = nrow(seats_transformed), byrow = FALSE)

nrow <- nrow(seats_transformed)
ncol <- ncol(seats_transformed)

stop <- FALSE

while (stop == FALSE){
  seats_transformed_current <- seats_transformed
  seats_transformed[is.na(seats_transformed)] <- 0

  # make shifted copies of the array
  shiftN = rbind(seats_transformed[-1,],rep(0,ncol))
  shiftS = rbind(rep(0,ncol),seats_transformed[-nrow,])
  shiftE = cbind( rep(0,nrow) , seats_transformed[,-ncol])
  shiftW = cbind(seats_transformed[,-1],rep(0,nrow))

  shiftSE = rbind(rep(0,ncol),cbind(rep(0,nrow-1),seats_transformed[-nrow, -ncol]))
  shiftSW = rbind(rep(0,ncol),cbind(seats_transformed[-nrow,-1],rep(0,ncol-1)))
  shiftNW = rbind(cbind(seats_transformed[-1,-1],rep(0,ncol-1)),rep(0,ncol))
  shiftNE = rbind(cbind(rep(0,ncol-1),seats_transformed[-1,-ncol]),rep(0,ncol))

  new_seats <- shiftW + shiftNW + shiftN + shiftNE + shiftE + shiftSE + shiftS + shiftSW

  new_seats[is.na(seats_transformed_current)] <- NA

  new_seats[new_seats == 0] <- -1 # fill an empty seat
  new_seats[new_seats >=  4] <- 0 # empty a seat if too many people are nearby
  new_seats[new_seats > 0 & seats_transformed == 0] <- 0 # empty seats don't get filled if there's an adjacent seat filled

  new_seats[new_seats != 0] <- 1

  if (sum(new_seats != seats_transformed_current, na.rm = TRUE) == 0){
    stop = TRUE
  }

  seats_transformed <- new_seats
  print(sum(seats_transformed == 1, na.rm = TRUE))
}

# number of occupied seats

sum(seats_transformed == 1, na.rm = TRUE)


# ### alternative part 1 which should help more with part 2
# seats_transformed <- seats %>%
#   separate(X1, paste("col", 1:nchar(seats$X1[1])), sep = "(?<=.)",extra = "drop") %>%
#   as.matrix()
#
# seats_transformed[seats_transformed == "L"] <- 0
# seats_transformed[seats_transformed == "."] <- NA
#
# seats_transformed <- matrix(as.numeric(seats_transformed), nrow = nrow(seats_transformed), byrow = FALSE)
#
# stop <- FALSE
#
# while (stop == FALSE){
#
#   new_seats <- matrix(NA, nrow = nrow(seats_transformed),
#                       ncol = ncol(seats_transformed))
#   seats_transformed_current <- seats_transformed
#   seats_transformed[is.na(seats_transformed)] <- 0
#
#   # add fake rows to end to avoid errors in calculation
#   seats_transformed_dummy <- rbind(seats_transformed, rep(0,ncol(seats_transformed)))
#   seats_transformed_dummy <- cbind(seats_transformed_dummy, rep(0,nrow(seats_transformed_dummy)))
#
#   for (i in 1:(nrow(seats_transformed))){
#     for (j in 1:nrow(seats_transformed)){
#
#       new_seats[i,j] <- sum(c(seats_transformed_dummy[i-1, j-1],
#                               seats_transformed_dummy[i-1, j],
#                               seats_transformed_dummy[i-1, j+1],
#                               seats_transformed_dummy[i, j-1],
#                               seats_transformed_dummy[i, j+1],
#                               seats_transformed_dummy[i+1, j-1],
#                               seats_transformed_dummy[i+1, j],
#                               seats_transformed_dummy[i+1, j+1]))
#     }
#   }
#
#   new_seats[is.na(seats_transformed_current)] <- NA
#
#   new_seats[new_seats == 0] <- -1 # fill an empty seat
#   new_seats[new_seats >=  4] <- 0 # empty a seat if too many people are nearby
#   new_seats[new_seats > 0 & seats_transformed == 0] <- 0 # empty seats don't get filled if there's an adjacent seat filled
#
#   new_seats[new_seats != 0] <- 1
#
#   if (sum(new_seats != seats_transformed_current, na.rm = TRUE) == 0){
#     stop = TRUE
#   }
#
#   seats_transformed <- new_seats
#   print(sum(seats_transformed == 1, na.rm = TRUE))
# }
#
# # number of occupied seats
# sum(seats_transformed == 1, na.rm = TRUE)

# part 2 - this time need to search each direction for first seat to determine whether it is empty or filled

seats_transformed <- seats %>%
  separate(X1, paste("col", 1:nchar(seats$X1[1])), sep = "(?<=.)",extra = "drop") %>%
  as.matrix()

seats_transformed[seats_transformed == "L"] <- 0
seats_transformed[seats_transformed == "."] <- NA

seats_transformed <- matrix(as.numeric(seats_transformed), nrow = nrow(seats_transformed), byrow = FALSE)

stop <- FALSE

# a fucntion to get the first non empty potision
get_seat_type <- function(position){
  return(position[!is.na(position)][1])
}

# rotate matrices
rotate <- function(x) t(apply(x, 2, rev))

while (stop == FALSE){

  new_seats <- matrix(NA, nrow = nrow(seats_transformed),
                      ncol = ncol(seats_transformed))

  seats_transformed_current <- seats_transformed

  # add fake rows to end to avoid errors in calculation
  seats_transformed_dummy <- rbind(rep(0,ncol(seats_transformed)), seats_transformed, rep(0,ncol(seats_transformed)))
  seats_transformed_dummy <- cbind(rep(0,nrow(seats_transformed_dummy)), seats_transformed_dummy, rep(0,nrow(seats_transformed_dummy)))

  for (i in 1:(nrow(seats_transformed))){
    for (j in 1:ncol(seats_transformed)){

        # hardcode in the sum
        # first seat seen to the right
        right <- get_seat_type(seats_transformed_dummy[i+1,(j+2):ncol(seats_transformed_dummy)])
        # first seat seen to the left
        left <- get_seat_type(rev(seats_transformed_dummy[i+1,1:(j)]))
        # first seat seen up
        up <- get_seat_type(rev(seats_transformed_dummy[1:i,j+1]))
        # first seat seen down
        down <- get_seat_type(seats_transformed_dummy[(i+2):nrow(seats_transformed_dummy),j+1])

        # first seat seen North West
        top_row <- (i+1) - min(j+1, i+1) + 1
        bottom_row <- i + 1
        left_col <- (j+1) - min(j+1, i+1) + 1
        right_col <- j + 1

        nw <- get_seat_type(rev(diag(seats_transformed_dummy[top_row:bottom_row, left_col:right_col]))[-1])

        # first seat seen North East
        top_row <- (i+2) - min(ncol(seats_transformed_dummy) - (j), i+1)
        bottom_row <- i + 1
        right_col <- (j) + min(ncol(seats_transformed_dummy) - (j), i+1)
        left_col <- j + 1

        ne <- get_seat_type(diag(rotate(seats_transformed_dummy[top_row:bottom_row, left_col:right_col]))[-1])

        # first seat seen South West
        top_row <- i + 1
        bottom_row <- (i+1) + min((j+1), nrow(seats_transformed_dummy) - (i+1))
        right_col <- j + 1
        left_col <- (j+1) - min((j+1), nrow(seats_transformed_dummy) - (i+1))

        sw <- get_seat_type(diag(rotate(rotate(rotate(seats_transformed_dummy[top_row:bottom_row, left_col:right_col]))))[-1])

        # first seat seen South East
        top_row <- i + 1
        bottom_row <- (i+1) + min(ncol(seats_transformed_dummy) - (j+1), nrow(seats_transformed_dummy) - (i+1))
        left_col <- j + 1
        right_col <- (j+1) + min(ncol(seats_transformed_dummy) -(j+1), nrow(seats_transformed_dummy) - (i+1))

        se <- get_seat_type(diag(seats_transformed_dummy[top_row:bottom_row, left_col:right_col])[-1])

        new_seats[i,j] <- up + down + left + right + sw + se + nw + ne
    }
  }

  new_seats[is.na(seats_transformed_current)] <- NA

  new_seats[new_seats == 0] <- -1 # fill an empty seat
  new_seats[new_seats >=  5] <- 0 # empty a seat if too many people are nearby
  new_seats[new_seats > 0 & seats_transformed == 0] <- 0 # empty seats don't get filled if there's an adjacent seat filled

  new_seats[new_seats != 0] <- 1

  if (sum(new_seats != seats_transformed_current, na.rm = TRUE) == 0){
    stop = TRUE
  }

  seats_transformed <- new_seats
  print(sum(seats_transformed == 1, na.rm = TRUE))
}

# number of occupied seats
sum(seats_transformed == 1, na.rm = TRUE)

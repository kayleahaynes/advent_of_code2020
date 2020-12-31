
# Decription of script ---------------------------------------------------------
# Day 22 advent of code -- crab combat
# ------------------------------------------------------------------------------

# set up -----------------------------------------------------------------------
library(tidyverse)
library(DescTools)

#%_% load data ------------------------------------------------------------------

player_1 <- readLines('day22_player1.txt')
player_2 <- readLines('day22_player2.txt')

# solve the puzzle
# part 1
while(length(player_1) > 0 & length(player_2) > 0){
  if(as.numeric(player_1[1]) > as.numeric(player_2[1])){
    player_1 <- c(player_1[-1], player_1[1], player_2[1])
    player_2 <- player_2[-1]
  } else {
    player_2 <- c(player_2[-1], player_2[1], player_1[1])
    player_1 <- player_1[-1]
  }
}

sum(as.numeric(player_1) * length(player_1):1)

# part 2 - used https://selbydavid.com/2020/12/06/advent-2020/#day22 to help understand the recursion with keeping track of cards played

player_1 <- readLines('day22_player1.txt')
player_2 <- readLines('day22_player2.txt')

recursive_combat <- function(player_1, player_2, subgame = FALSE){

  played <- NULL # keep track of played orders for player 1 to avoid infinite recursion

  while(length(player_1) > 0 & length(player_2) > 0){

    if(as.numeric(player_1[1]) <= length(player_1)-1 & as.numeric(player_2[1]) <= length(player_2)-1){
      rc <- recursive_combat(player_1[-1][1:player_1[1]],player_2[-1][1:player_2[1]], subgame = TRUE)

      if(rc == "player_1"){
        player_1 <- c(player_1[-1], player_1[1], player_2[1])
        player_2 <- player_2[-1]
      } else {
        player_2 <- c(player_2[-1], player_2[1], player_1[1])
        player_1 <- player_1[-1]
      }
    } else{

      config <- paste(player_1, player_2, collapse = ',', sep = '|')

      # if there was a previous round in this game that had exactly the same cards in the same order in the same players' decks, the game instantly ends in a win for player 1
      #
      if (config %in% played) return("player_1")
      played <- c(played, config)

      if(as.numeric(player_1[1]) > as.numeric(player_2[1])){
        player_1 <- c(player_1[-1], player_1[1], player_2[1])
        player_2 <- player_2[-1]
      } else {
        player_2 <- c(player_2[-1], player_2[1], player_1[1])
        player_1 <- player_1[-1]
      }
    }
  }

  if(length(player_1) > 0){
    winner <- "player_1"
  } else{
    winner <- "player_2"
  }

  if(subgame){
    return(winner)
  } else{
    return(list(player_1, player_2))
  }
}

recursive_combat_game <- recursive_combat(as.numeric(player_1), as.numeric(player_2), FALSE)

sum(as.numeric(recursive_combat_game[[1]]) * length(recursive_combat_game[[1]]):1)



# p1 <-list()
# p2 <- list()
#
# # keep all player 1 to check for infinite recursion
#
# all_p1 <- list()
#
# round <- 0

# recursive_combat <- function(player_1, player_2){
#   while(length(player_1) > 0 & length(player_2) > 0){
#     round <- round + 1
#     print(round)
#     if(length(all_p1) > 0){
#       for (i in length(all_p1)){
#         if(all.equal(as.numeric(player_1), as.numeric(all_p1[[i]])) == TRUE){
#           return(list(player_1, player_2))
#         }
#       }
#     }
#
#     all_p1 <- c(list(player_1), all_p1)
#
#     combat_rounds <- combat(player_1, player_2)
#
#     if(length(combat_rounds[[1]]) == 0 | length(combat_rounds[[2]]) == 0){
#
#       if(length(p1) == 0 & length(p2) == 0){
#         return(combat_rounds)
#       }
#
#       if(combat_rounds[[3]] == "player_1"){
#         player_1 <- c(p1[[1]][-1], p1[[1]][1], p2[[1]][1])
#         player_2 <- p2[[1]][-1]
#       } else {
#         player_2 <- c(p2[[1]][-1], p2[[1]][1], p1[[1]][1])
#         player_1 <- p1[[1]][-1]
#       }
#       p1 <- p1[-1]
#       p2 <- p2[-1]
#     } else if (combat_rounds[[1]][1] > (length(combat_rounds[[1]])-1) | combat_rounds[[2]][1] > (length(combat_rounds[[2]])-1)){
#       player_1 <- c(p1[[1]][-1], p1[[1]][1], p2[[1]][1])
#       player_2 <- p2[[1]][-1]
#
#       p1 <- p1[-1]
#       p2 <- p2[-1]
#     } else{
#
#       p1 <- c(list(as.numeric(combat_rounds[[1]])), p1)
#       p2 <- c(list(as.numeric(combat_rounds[[2]])), p2)
#
#       player_1 <- p1[[1]][2:(p1[[1]][1]+1)]
#       player_2 <- p2[[1]][2:(p2[[1]][1]+1)]
#     }
#   }
# }



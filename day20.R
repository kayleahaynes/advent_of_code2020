# Decription of script ---------------------------------------------------------
# Day 20 advent of code -- Jurassic Jigsaw
# ------------------------------------------------------------------------------

# set up -----------------------------------------------------------------------
library(tidyverse)
library(DescTools)

#%_% load data --------------------------------------------------------------------

pieces <- read_table('day20.txt', col_names = FALSE)

piece_numbers <- pieces %>%
  filter(X1 %like% "Tile%") %>%
  mutate(numbers = parse_number(X1))

pieces_transformed <- pieces %>%
  filter(!X1 %like% "Tile%") %>%
  separate(X1, paste("col", 1:nchar(pieces$X1[1])), sep = "(?<=.)",extra = "drop") %>%
  mutate(piece_number = rep(piece_numbers$numbers, each = 10))

# find which parts of the piece are #
pieces_transformed <- pieces_transformed %>%
  pivot_longer(!piece_number) %>%
  group_by(piece_number, name) %>%
  mutate(row_number = 1:n()) %>%
  ungroup() %>%
  mutate(col_number = parse_number(name)) %>%
  filter(value == "#")

# only interested in edge pieces

edges <- pieces_transformed %>%
  filter(row_number %in% c(1,10) | col_number %in% c(1,10))

current_rotation <- edges %>%
  mutate(layout = "current")

# flip over x axis
edges_flip_x_top_bottom <- edges %>%
  filter(row_number %in% c(1,10)) %>%
  mutate(row_number = ifelse(row_number == 1, 10, 1)) %>%
  mutate(layout = "flip_x")

edges_flip_x_left_right <- edges %>%
  filter(col_number %in% c(1,10)) %>%
  mutate(row_number = abs(11-row_number)) %>%
  mutate(layout = "flip_x")

edges_flip_x<- unique(bind_rows(edges_flip_x_top_bottom, edges_flip_x_left_right))

# flip over y axis
edges_flip_y_left_right <- edges %>%
  filter(col_number %in% c(1,10)) %>%
  mutate(col_number = ifelse(col_number == 1, 10, 1)) %>%
  mutate(layout = "flip_y")

edges_flip_y_top_bottom <- edges %>%
  filter(row_number %in% c(1,10)) %>%
  mutate(col_number = abs(11-col_number)) %>%
  mutate(layout = "flip_y")

edges_flip_y <- unique(bind_rows(edges_flip_y_left_right , edges_flip_y_top_bottom))

# rotations
# clockwise 90

edge_rotate_clockwise_90 <- edges %>%
  mutate(col_number2 = case_when(row_number == 1 ~ 10,
                                 col_number == 10 ~ abs(11-row_number),
                                 row_number == 10 ~ 1,
                                 col_number == 1 ~ abs(11-row_number)),
         row_number2 = case_when(row_number == 1 ~ col_number,
                                 col_number == 10 ~ 10,
                                 row_number == 10 ~ col_number,
                                 col_number == 1 ~ 1)) %>%
  select(piece_number, name, value, row_number = row_number2, col_number = col_number2) %>%
  mutate(layout = "rotate_clockwise_90")

edge_rotate_clockwise_180 <- edges %>%
  mutate(col_number2 = case_when(row_number == 1 ~ abs(11-col_number),
                                 col_number == 10 ~ 1,
                                 row_number == 10 ~ abs(11-col_number),
                                 col_number == 1 ~ 10),
         row_number2 = case_when(row_number == 1 ~ 10,
                                 col_number == 10 ~ abs(11-row_number),
                                 row_number == 10 ~ 1,
                                 col_number == 1 ~ abs(11-row_number))) %>%
  select(piece_number, name, value, row_number = row_number2, col_number = col_number2) %>%
  mutate(layout = "rotate_clockwise_180")

edge_rotate_clockwise_270 <- edges %>%
  mutate(col_number2 = case_when(row_number == 1 ~ 1,
                                 col_number == 10 ~ as.numeric(row_number),
                                 row_number == 10 ~ 10,
                                 col_number == 1 ~ as.numeric(row_number)),
         row_number2 = case_when(row_number == 1 ~ abs(11-col_number),
                                 col_number == 10 ~ 1,
                                 row_number == 10 ~ abs(11-col_number),
                                 col_number == 1 ~ 10)) %>%
  select(piece_number, name, value, row_number = row_number2, col_number = col_number2) %>%
  mutate(layout = "rotate_clockwise_270")

# flip and rotate
#
edge_rotate_clockwise_90_flipx <- edges_flip_x %>%
  mutate(col_number2 = case_when(row_number == 1 ~ 10,
                                 col_number == 10 ~ abs(11-row_number),
                                 row_number == 10 ~ 1,
                                 col_number == 1 ~ abs(11-row_number)),
         row_number2 = case_when(row_number == 1 ~ col_number,
                                 col_number == 10 ~ 10,
                                 row_number == 10 ~ col_number,
                                 col_number == 1 ~ 1)) %>%
  select(piece_number, name, value, row_number = row_number2, col_number = col_number2) %>%
  mutate(layout = "rotate_clockwise_90_flipx")

edge_rotate_clockwise_180_flipx <- edges_flip_x %>%
  mutate(col_number2 = case_when(row_number == 1 ~ abs(11-col_number),
                                 col_number == 10 ~ 1,
                                 row_number == 10 ~ abs(11-col_number),
                                 col_number == 1 ~ 10),
         row_number2 = case_when(row_number == 1 ~ 10,
                                 col_number == 10 ~ abs(11-row_number),
                                 row_number == 10 ~ 1,
                                 col_number == 1 ~ abs(11-row_number))) %>%
  select(piece_number, name, value, row_number = row_number2, col_number = col_number2) %>%
  mutate(layout = "rotate_clockwise_180_flipx")

edge_rotate_clockwise_270_flipx <- edges_flip_x %>%
  mutate(col_number2 = case_when(row_number == 1 ~ 1,
                                 col_number == 10 ~ row_number,
                                 row_number == 10 ~ 10,
                                 col_number == 1 ~ row_number),
         row_number2 = case_when(row_number == 1 ~ abs(11-col_number),
                                 col_number == 10 ~ 1,
                                 row_number == 10 ~ abs(11-col_number),
                                 col_number == 1 ~ 10)) %>%
  select(piece_number, name, value, row_number = row_number2, col_number = col_number2) %>%
  mutate(layout = "rotate_clockwise_270_flipx")

# flip and rotate
#
edge_rotate_clockwise_90_flipy <- edges_flip_y %>%
  mutate(col_number2 = case_when(row_number == 1 ~ 10,
                                 col_number == 10 ~ abs(11-row_number),
                                 row_number == 10 ~ 1,
                                 col_number == 1 ~ abs(11-row_number)),
         row_number2 = case_when(row_number == 1 ~ col_number,
                                 col_number == 10 ~ 10,
                                 row_number == 10 ~ col_number,
                                 col_number == 1 ~ 1)) %>%
  select(piece_number, name, value, row_number = row_number2, col_number = col_number2) %>%
  mutate(layout = "rotate_clockwise_90_flipy")

edge_rotate_clockwise_180_flipy <- edges_flip_y %>%
  mutate(col_number2 = case_when(row_number == 1 ~ abs(11-col_number),
                                 col_number == 10 ~ 1,
                                 row_number == 10 ~ abs(11-col_number),
                                 col_number == 1 ~ 10),
         row_number2 = case_when(row_number == 1 ~ 10,
                                 col_number == 10 ~ abs(11-row_number),
                                 row_number == 10 ~ 1,
                                 col_number == 1 ~ abs(11-row_number))) %>%
  select(piece_number, name, value, row_number = row_number2, col_number = col_number2) %>%
  mutate(layout = "rotate_clockwise_180_flipy")

edge_rotate_clockwise_270_flipy <- edges_flip_y %>%
  mutate(col_number2 = case_when(row_number == 1 ~ 1,
                                 col_number == 10 ~ as.numeric(row_number),
                                 row_number == 10 ~ 10,
                                 col_number == 1 ~ as.numeric(row_number)),
         row_number2 = case_when(row_number == 1 ~ abs(11-col_number),
                                 col_number == 10 ~ 1,
                                 row_number == 10 ~ abs(11-col_number),
                                 col_number == 1 ~ 10)) %>%
  select(piece_number, name, value, row_number = row_number2, col_number = col_number2) %>%
  mutate(layout = "rotate_clockwise_270_flipy")

all_layouts <- bind_rows(current_rotation,
                         edges_flip_x,
                         edges_flip_y,
                         edge_rotate_clockwise_90,
                         edge_rotate_clockwise_180,
                         edge_rotate_clockwise_270,
                         edge_rotate_clockwise_90_flipx,
                         edge_rotate_clockwise_180_flipx,
                         edge_rotate_clockwise_270_flipx,
                         edge_rotate_clockwise_90_flipy,
                         edge_rotate_clockwise_180_flipy,
                         edge_rotate_clockwise_270_flipy
)

all_layouts_top <- all_layouts %>%
  filter(row_number == 1) %>%
  mutate(row_join = 10,
         col_join = col_number) %>%
  mutate(side = 1)

all_layouts_right <- all_layouts %>%
  filter(col_number == 10) %>%
  mutate(col_join = 1,
         row_join = row_number) %>%
  mutate(side = 2)

all_layouts_bottom <- all_layouts %>%
  filter(row_number == 10) %>%
  mutate(row_join = 1,
         col_join = col_number) %>%
  mutate(side = 3)

all_layouts_left <- all_layouts %>%
  filter(col_number == 1) %>%
  mutate(col_join = 10,
         row_join = row_number) %>%
  mutate(side = 4)

required_join <- bind_rows(all_layouts_top,
                           all_layouts_left,
                           all_layouts_right,
                           all_layouts_bottom)

# check if joins exist anywhere
matched_pieces <- NULL

for (piece_number_i in unique(required_join$piece_number)){

  print(piece_number_i)
  # first check if the top row joins to any other piece
  for (check_side in c(1:4)){

    check_piece <- required_join %>%
      filter(piece_number == piece_number_i, side == check_side)

    # use the current rotation of the piece of interest and compare this to the other pieces rotated and flipped
    check_piece_layout <- check_piece %>%
      filter(layout == "current")

    check_all <- required_join %>%
      filter(side == c(3,4,1,2)[check_side]) %>%
      filter(piece_number != unique(check_piece_layout$piece_number)) %>%
      group_by(piece_number, row_number, layout) %>%
      mutate(number_row_pieces = n()) %>%
      group_by(piece_number, col_number, layout) %>%
      mutate(number_col_pieces = n()) %>%
      filter((row_number %in% c(1,10) & number_row_pieces == nrow(check_piece_layout)) | (col_number %in% c(1,10) & number_col_pieces == nrow(check_piece_layout))) %>%
      ungroup() %>%
      filter(paste(row_number, col_number) %in% paste(check_piece_layout$row_join, check_piece_layout$col_join)) %>%
      group_by(piece_number, layout) %>%
      count() %>%
      filter(n == nrow(check_piece_layout))

    if (nrow(check_all) > 0){
      matched_pieces <- bind_rows(matched_pieces, data.frame(piece_number_i, check_side, check_all$piece_number, check_all$layout))
    }
  }
}

matched_pieces %>%
  select(piece_number_i, check_side) %>%
  distinct() %>%
  group_by(piece_number_i) %>%
  summarise(count = n()) %>%
  group_by(piece_number_i) %>%
  summarise(max_count = max(count)) %>%
  filter(max_count == 2) %>%
  select(piece_number_i) %>%
  prod() %>%
  as.character()

# part 2 -----------------------------------------------------------------------

# first make up the puzzle

# start with 1367
edges <- c(1367, 2239, 2833, 3359) # from part 1
# edges <- c(1171. 1951, 2971, 3079)
already_placed_pieces <- 1367
already_placed_pieces_rotation <- "current"

piece_i <- 1367

check_side_i <- 2

find_match <- matched_pieces %>%
  filter(piece_number_i == piece_i, check_side == check_side_i) %>%
  select(check_all.piece_number, check_all.layout) %>%
  head(1) # some rotations result in the same piece so just choose the top 1

piece_i <- find_match$check_all.piece_number
layout_i <- find_match$check_all.layout

for (i in 1:(nrow(piece_numbers)-1)){

  print(i)

  if (piece_i %in% edges | length(already_placed_pieces) %% sqrt(nrow(piece_numbers)) == sqrt(nrow(piece_numbers))-1){

    # if the check side i is already 3 then we've reached the last row
    if(check_side_i != 3){
      check_side_old <- check_side_i
      check_side_i <- 3
    }
  }

  if(length(already_placed_pieces) > 1 & (tail(already_placed_pieces,1) %in% edges | length(already_placed_pieces) %% sqrt(nrow(piece_numbers)) == 0)){
    check_side_i <- ifelse(check_side_old == 2, 4, 2)
  }

  check_piece <-  required_join %>%
    filter(piece_number == piece_i, side == check_side_i, layout == layout_i)

  check_all <- required_join %>%
    filter(!piece_number %in% already_placed_pieces) %>%
    filter(piece_number != unique(check_piece$piece_number)) %>%
    group_by(piece_number, layout, side) %>%
    filter(all(paste(check_piece$row_join, check_piece$col_join) %in% paste(row_number, col_number)) & all(paste(row_number, col_number) %in% paste(check_piece$row_join, check_piece$col_join))) %>%
    head(1)

  already_placed_pieces <- c(already_placed_pieces, piece_i)
  already_placed_pieces_rotation <- c(already_placed_pieces_rotation, layout_i)
  piece_i <- check_all$piece_number
  layout_i <- check_all$layout
}

# add the last piece
already_placed_pieces <- c(already_placed_pieces, piece_i)
already_placed_pieces_rotation <- c(already_placed_pieces_rotation, layout_i)

# now that I've found the order of all the pieces make up the puzzle

# create a function to flip and rotate the pieces, only coded up the unique rotations that I need
# unique(already_placed_pieces_rotation)
npieces <- sqrt(nrow(piece_numbers))

flip_rotate <- function(piece, rotation){

  # remove the border of the piece
  piece <- piece %>%
    filter(row_number > 1 & row_number < 10 & col_number > 1 & col_number < 10) %>%
    select(piece_number, row_number, col_number)

  if(rotation == "current"){
    rotated_piece <- piece
  } else if(rotation == "flip_y"){
    rotated_piece <- piece %>%
      mutate(col_number = abs(11-col_number))
  } else if(rotation == "flip_x"){
    rotated_piece <- piece %>%
      mutate(row_number = abs(11-row_number))
  } else if(rotation == "rotate_clockwise_90"){
    rotated_piece <- piece %>%
      mutate(col_number2 = abs(11-row_number),
             row_number2 = col_number)  %>%
      select(piece_number, row_number = row_number2, col_number = col_number2)
  } else if(rotation == "rotate_clockwise_180"){
    rotated_piece <- piece %>%
      mutate(col_number2 = abs(11-col_number),
             row_number2 = abs(11-row_number))  %>%
      select(piece_number, row_number = row_number2, col_number = col_number2)
  } else if(rotation == "rotate_clockwise_180"){
    rotated_piece <- piece %>%
      mutate(col_number2 = row_number,
             row_number2 = abs(11-col_number))  %>%
      select(piece_number, row_number = row_number2, col_number = col_number2)
  } else if(rotation == "rotate_clockwise_90_flipx"){
    rotated_piece <- piece %>%
      mutate(row_number = abs(11-row_number))  %>%
      mutate(col_number2 = abs(11-row_number),
             row_number2 = col_number)  %>%
      select(piece_number, row_number = row_number2, col_number = col_number2)
  } else if(rotation == "rotate_clockwise_270_flipx"){
    rotated_piece <- piece %>%
      mutate(row_number = abs(11-row_number))  %>%
      mutate(col_number2 = row_number,
             row_number2 = abs(11-col_number))  %>%
      select(piece_number, row_number = row_number2, col_number = col_number2)
  } else if(rotation == "rotate_clockwise_270"){
    rotated_piece <- piece %>%
      mutate(col_number2 = row_number,
             row_number2 = abs(11-col_number))  %>%
      select(piece_number, row_number = row_number2, col_number = col_number2)
  }

  rotated_piece <- rotated_piece %>%
    mutate(row_number = row_number - 1,
           col_number = col_number - 1)
  return(rotated_piece)
}

jigsaw <- NULL
jigsaw_rotate <- NULL

for (i in 1:(length(already_placed_pieces)/npieces)){
  if(i %% 2 == 1){
  jigsaw <- rbind(jigsaw, already_placed_pieces[(((i-1)*npieces) + 1) : ((i)*npieces)])
  jigsaw_rotate <- rbind(jigsaw_rotate, already_placed_pieces_rotation[(((i-1)*npieces) + 1) : ((i)*npieces)])
  }
  else {
  jigsaw <- rbind(jigsaw, rev(already_placed_pieces[(((i-1)*npieces) + 1) : ((i)*npieces)]))
  jigsaw_rotate <- rbind(jigsaw_rotate, rev(already_placed_pieces_rotation[(((i-1)*npieces) + 1) : ((i)*npieces)]))
  }
}

all_pieces_together <-  NULL

for (row_i in 1:nrow(jigsaw)){
  for (col_i in 1:ncol(jigsaw)){
    piece <- pieces_transformed %>%
      filter(piece_number == jigsaw[row_i, col_i])

    piece_flipped_rotation <- flip_rotate(piece, jigsaw_rotate[row_i, col_i]) %>%
      mutate(row_number = row_number + (row_i - 1)*8,
             col_number = col_number + (col_i - 1)*8)

    all_pieces_together <- bind_rows(all_pieces_together, piece_flipped_rotation)
  }
}

all_pieces_together %>%
  ggplot() +
  geom_point(aes(x = col_number, y = row_number)) +
  scale_y_reverse()

# search for the sea monster
sea_monster <- data.frame(row_number = c(1,2,2,2,2,2,2,2,2,3,3,3,3,3,3) ,
                          col_number = c(19, 1,6,7,12,13,18,19,20, 2,5,8,11,14,17),
                          rotate = "current")

k <- 1
monster_position <- NULL
n1 <- max(all_pieces_together$col_number)  + 1

rotated_puzzle <- all_pieces_together %>%
  #  mutate(col_number = abs(n1-col_number))   # flip_y
  # mutate(row_number = abs((n1 - row_number))) #flip_x
  # mutate(col_number2 = abs(n1-row_number),
  #        row_number2 = col_number,
  #        rotate = "rotate_clockwise90") %>%
  # select(row_number = row_number2, col_number = col_number2)
mutate(col_number2 = abs(n1-col_number),
       row_number2 = abs(n1-row_number),
       rotate = "rotate_clockwise180") %>%
select(row_number = row_number2, col_number = col_number2)
# mutate(col_number2 = row_number,
#        row_number2 = abs(97-col_number),
#        rotate = "rotate_clockwise_270")  %>%
# select(row_number = row_number2, col_number = col_number2)
# mutate(row_number = abs(97-row_number))  %>%
#   mutate(col_number2 = abs(97-row_number),
#          row_number2 = col_number,
#          rotate = "rotate_clockwise_90_flipx") %>%
#   select(row_number = row_number2, col_number = col_number2)

# mutate(row_number = abs(97-row_number))  %>%
#   mutate(col_number2 = abs(97-col_number),
#                  row_number2 = abs(97 - row_number),
#          rotate = "rotate_clockwise_180_flipx") %>%
#   select(row_number = row_number2, col_number = col_number2)
#
# mutate(row_number = abs(97-row_number))  %>%
#   mutate(col_number2 = row_number,
#          row_number2 = abs(97-col_number),
#          rotate = "rotate_clockwise_270_flipx") %>%
#   select(row_number = row_number2, col_number = col_number2)
# mutate(col_number = abs(97-col_number))  %>%
#     mutate(col_number2 = abs(97-row_number),
#            row_number2 = col_number,
#            rotate = "rotate_clockwise_90_flipy") %>%
#     select(row_number = row_number2, col_number = col_number2)
#
# mutate(col_number = abs(97-col_number))  %>%
#   mutate(col_number2 = abs(97-col_number),
#          row_number2 = abs(97 - row_number),
#          rotate = "rotate_clockwise_180_flipy") %>%
#   select(row_number = row_number2, col_number = col_number2)
#
# mutate(col_number = abs(97-col_number))  %>%
#   mutate(col_number2 = abs(97-col_number),
#                  row_number2 = abs(97 - row_number),
#          rotate = "rotate_clockwise_270_flipy") %>%
#   select(row_number = row_number2, col_number = col_number2)


for(row_i in 1:(max(rotated_puzzle$row_number))){
  print(row_i)
  for(col_i in 1:(max(rotated_puzzle$col_number))){

    relative_sea_monster_position <- sea_monster %>%
      mutate(row_number = row_number + (row_i - 1),
             col_number = col_number + (col_i - 1))

    # check if it exists

    check_monster <- rotated_puzzle %>%
      merge(relative_sea_monster_position, by = c("row_number", "col_number"))

    check_monster2 <- check_monster %>%
      group_by(rotate) %>%
      mutate(count = n()) %>%
      filter(count == nrow(sea_monster))

    if(nrow(check_monster2) > 0){
      print(paste(row_i, col_i))
      monster_position <- rbind(monster_position, data.frame(check_monster2))
    }
  }
}

rotated_puzzle %>%
  ggplot() +
  geom_point(aes(x = col_number, y = row_number)) +
  geom_point(data = monster_position, aes(x = col_number, y = row_number), col = "red")+
  scale_y_reverse()

nrow(all_pieces_together) - nrow(monster_position)

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

    for (layout_i in unique(check_piece$layout)){

      check_piece_layout <- check_piece %>%
        filter(layout == layout_i)

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
      matched_pieces <- bind_rows(matched_pieces, data.frame(piece_number_i, layout_i, check_side, check_all$piece_number, check_all$layout))
      }
    }
  }
}

matched_pieces %>%
  select(piece_number_i, layout_i, check_side) %>%
  distinct() %>%
  group_by(piece_number_i, layout_i) %>%
  summarise(count = n()) %>%
  group_by(piece_number_i) %>%
  summarise(max_count = max(count)) %>%
  filter(max_count == 2) %>%
  select(piece_number_i) %>%
  prod() %>%
  as.character()

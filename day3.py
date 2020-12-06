# Description ------------------------------------------------------------------
# Advent of code day 2 -- find the valid passwords based on different policies 
# Attempted in R first and then converted into python. For this one I used a better 
# solution that I found online in R to convert to python. 

# set up -----------------------------------------------------------------------

# load data --------------------------------------------------------------------
def split(word): 
    return [char for char in word.rstrip('\n')]  

with open('day3.txt') as file:
    path = [split(text) for text in file]

# solve the puzzle -------------------------------------------------------------
# Part 1 count the number of trees encountered if you take the path
# 3 across and 1 down

route_row = [1 + x for x in range(len(path)-1)]
route_col = [(3 * x) % len(path[1]) for x in range(1,len(path))]

number_of_trees = 0
for i in route_row:
    number_of_trees += path[i][route_col[i-1]] == '#'

# Part 2 # now try different combinations of slopes
# Right 1, down 1.
# Right 3, down 1. (This is the slope you already checked.)
# Right 5, down 1.
# Right 7, down 1.
# Right 1, down 2.

def traverse(right, down):
    route_row = [1 + x for x in range(round(len(path)/down)-1)]
    route_col = [(right * x) % len(path[1]) for x in range(1,round(len(path)/down))]

    number_of_trees = 0
    for i in route_row:
        number_of_trees += path[i*down][route_col[i-1]] == '#'
    return number_of_trees

traverse(1,1) * traverse(3,1) * traverse(5,1) * traverse(7,1) * traverse(1,2)


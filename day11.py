# advent of code day 11. This was @ASpittel solution which 
# iteratively checks all combinations for each seat until 
# either a seat which is empty or filled is found or there are no valid seats 


# check if the seats are still within the boundary (i.e. the indices aren't too high or too low)
def check_valid_seat(seats, row, col):
    if row < 0 or col < 0:
        return False
    if row >= len(seats):
        return False
    if col >= len(seats[row]):
        return False
    return True

# check if the next valid seat is empty or taken 
def check_seat(seats, row_num, col_num, rd, cd):
    row_num += rd
    col_num += cd
    if not check_valid_seat(seats, row, col): return False
    while seats[row][col] == '.':
        row += rd
        col += cd
        if not check_valid_seat(seats, row, col): return False
    if seats[row][col] =='L':
        return False
    if seats[row][col] =='#':
        return True
    return False


def count_open(seats, row_num, col_num):
    count = 0
    moves = [0, 1, -1]
    for rd in moves:
        for cd in moves:
            if rd != 0 or cd != 0:
                if check_seat(seats, row_num, col_num, rd, cd):
                    count += 1
    return count
    

def str_seats(seats):
    str_seats = [''.join(line) for line in seats]
    return ''.join(str_seats)


def pretty_print(seats):
    for row in seats:
        print(''.join(row))
    print('----------------')


with open('day11.txt') as file:
    seats = [list(row.replace('L', '#').strip()) for row in file]
    new_seats = [x[:] for x in seats]
    counts = [x[:] for x in seats]
    times = 0
    while True:
        changes = 0
        for row_num, row in enumerate(seats):
            for col_num, col in enumerate(row):
                if col != '.':
                    count = count_open(seats, row_num, col_num)
                    counts[row_num][col_num] = str(count)
                    if col == '#' and count >= 5:
                        new_seats[row_num][col_num] = 'L'
                        changes += 1
                    if col == 'L' and count == 0:
                        new_seats[row_num][col_num] = '#'
                        changes += 1
        if changes == 0:
            print(str_seats(seats).count('#'))
            break
        times += 1
        changes = 0
        seats = [x[:] for x in new_seats]
        counts = [x[:] for x in new_seats]
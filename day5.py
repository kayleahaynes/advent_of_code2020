# Decription of script ---------------------------------------------------------
# Day 5 advent of code -- find seat number on plane
# ------------------------------------------------------------------------------

# solve puzzle ------------------------------------------------------------------
# part 1

# idea to use binary number from @zakvarty 

def seat_search(li, st, n):
    current = 0
    for i in range(len(li)):
        current += (li[i] == 'B' or li[i] == 'R') * 2**(n - (st + i))
    return(current)

with open('day5.txt') as boarding_pass:
    seat_ids = []
    
    for line in boarding_pass:
       row = seat_search(line[0:7], 0, 6) 
       col = seat_search(line[7:], 7, 9) 
       seat_id = row * 8 + col
       seat_ids.append(seat_id)

max(seat_ids)

# part 2 (copied from @@ASpittel) 

seat_ids = sorted(seat_ids)
prev = seat_ids[0]
for seat_id in seat_ids[1:]:
    if prev != seat_id - 1:
        print("Part 2", prev)
        break
    prev += 1
       


# Decription of script ---------------------------------------------------------
# Day 6 advent of code -- customs questionairres -- what questions were answered 
# and which ones were answered by the whole group
# 
# For this example I went through @ASpittel's solution 
# ------------------------------------------------------------------------------

from collections import Counter
import numpy as np

# solve puzzle ------------------------------------------------------------------

def get_group_count(group):
    group_count = 0
    # count how many times a letter appears in the group
    letter_counts = Counter(group)
    # if the number of times a letter appears is equal to the group size then 
    # all of the group answered yes 
    for letter in letter_counts:
        if letter_counts[letter] == group_size: 
            group_count += 1
    return group_count


with open('day6.txt') as custom_forms:
    # initialise groups and group size 
    group = []
    group_all = []
    group_any = []
    group_size = 0

    for line in custom_forms:
        
        if line != "\n" : # separate groups are on separate lines 
            group_size += 1
            for letter in line.rstrip():
                group.append(letter) 
        else: 
            group_all.append(get_group_count(group)) 
            group_any.append(len(set(group))) # count if anyone has answered a question  (this gets the unique qs)

            group_size = 0 #reset group 
            group = [] # reset group 

    # add the last group 
    group_all.append(len(set(group)))       
    group_any.append(get_group_count(group))

    print("Part 1", sum(group_any))
    print("Part 2", sum(group_all))

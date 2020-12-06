# set up -----------------------------------------------------------------------
import pandas as pd
import numpy as np

# load data --------------------------------------------------------------------
spend_report = pd.read_csv('day1.csv', header = None)  
spend_report = spend_report.rename(columns = {0:'cost'})

# solve the puzzle -------------------------------------------------------------
# Part 1 (which 2 numbers sum to 2020 -- multiply these together)
stop = False 

for i in range(len(spend_report)):
    for j in range(len(spend_report)):
        if(spend_report['cost'][i] + spend_report['cost'][j] == 2020):
            print(spend_report['cost'][i] * spend_report['cost'][j])
            stop = True
            break
        if (stop):
            break

# Part 2 (which 3 numbers sum to 2020 -- multiply these together)
stop = False 

for i in range(len(spend_report)):
    for j in range(len(spend_report)):
        if(spend_report['cost'][i] + spend_report['cost'][j] <= 2020):
            for k in range(len(spend_report)):
                if(spend_report['cost'][i] + spend_report['cost'][j] + spend_report['cost'][k] == 2020):
                    print(spend_report['cost'][i] * spend_report['cost'][j] * spend_report['cost'][k])
                    stop = True
                    break
            if (stop):
                break
        if (stop):
            break

    
# An alternative neat solution found online (@ASpittal) -------------------------------------------

def part_one(nums, set_nums):
    for num in nums:
        if 2020 - num in set_nums:
            return num * (2020 - num)


def part_two(nums, set_nums):
    for num in nums:
        for num2 in nums:
            if (2020 - num - num2) in set_nums:
                return num * (2020 - num - num2) * num2



nums = [int(line) for line in spend_report['cost']]
set_nums = set(nums)

print("Part 1", part_one(nums, set_nums))
print("Part 2", part_two(nums, set_nums))
    
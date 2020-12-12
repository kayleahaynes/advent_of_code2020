# Decription of script ---------------------------------------------------------
# Day 10 advent of code 
# This solution comes from @Brotherluii which is a very neat way to do it 
# ------------------------------------------------------------------------------

# solve puzzle ------------------------------------------------------------------

with open('day10.txt') as file:
    data = sorted([int(line) for line in file])

data.append(data[-1] + 3)

start = 0 
difs = ''
for jolt in data:
    diff = jolt - start
    difs += str(diff)
    start = jolt 

difs = difs.split('3')
print(2**difs.count('11') * 4**difs.count('111')* 7**difs.count('1111'))
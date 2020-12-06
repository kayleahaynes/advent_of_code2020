# Description ------------------------------------------------------------------
# Advent of code day 2 -- find the valid passwords based on different policies 
# Attempted in R first and then converted into python. For this one I used a better 
# solution that I found online in R to convert to python. 

# set up -----------------------------------------------------------------------
import pandas as pd
import re

# load data --------------------------------------------------------------------
passwords = pd.read_csv('day2.csv', header = None)  
passwords = passwords.rename(columns = {0:'policies'})

# solve the puzzle -------------------------------------------------------------
# Part 1 Part 1: How many passwords are valid according to their policies (letter must appear greater than the minimum occurance and less than the maximum occurance).
# use string matching to extract the different parts of the files related to the password and the policies. Count how many times a letter appears in a password and filter out invalid passwords

passwords_transformed = passwords['policies'].str.extract('(?P<min>\d+)-(?P<max>\d+) (?P<letter>[^:]*): (?P<password>.*)', expand = False)

count = []
for i in range(len(passwords_transformed)):
    count.append(str.count(passwords_transformed['password'][i], passwords_transformed['letter'][i]))
passwords_transformed['count'] = count

number_valid_passwords = sum((passwords_transformed['count'] >= passwords_transformed['min'].astype(int)) & (passwords_transformed['count'] <= passwords_transformed['max'].astype(int)))

# Part 2 How many passwords are valid according to their policies where a letter must appear in either one of the positions (but not both).

in_position = []
for i in range(len(passwords_transformed)):
    in_position.append((passwords_transformed['password'][i][int(passwords_transformed['min'][i])-1:int(passwords_transformed['min'][i])] == passwords_transformed['letter'][i]) + (passwords_transformed['password'][i][int(passwords_transformed['max'][i])-1:int(passwords_transformed['max'][i])] == passwords_transformed['letter'][i]))

passwords_transformed['in_position'] = in_position
number_of_valid_passwords = sum(passwords_transformed['in_position'] == 1)


# An alternative neat solution found online (@ASpittal) ------------------------------------------------

with open('day2.csv') as file:
    letters = [num.split(": ") for num in file]
    tally = 0
    tally2 = 0
        for policy, password in letters:
            length, letter = policy.split(" ")
            start, stop = length.split("-")
            start, stop = int(start), int(stop)

     # part 1
        if password.count(letter) >= start and password.count(letter) <= stop:
            tally += 1

        # part 2
        start, stop = start - 1, stop - 1
        try:
            if (password[start] == letter or password[stop] == letter) and (password[start] != password[stop]):
                tally2 += 1
        except:
            pass
        

    print(tally)
    print(tally2)




   
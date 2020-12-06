# Description ------------------------------------------------------------------
# Advent of code day 4 -- validate passports 
# I attempted this in R first then found this solution by @noahlet which I really 
# liked so decided to try and use it here instead of going through all of the rules again


# set up -----------------------------------------------------------------------
import pandas as pd
import re

# load data --------------------------------------------------------------------

passports = open('day4.txt')
each_passport = passports.read().split('\n\n')

# solve the puzzle -------------------------------------------------------------
# part 1 Passports should all have 8 fields unless cid is missing in which case count it as valid
fields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

sum([all([re.search(pattern, passport) for pattern in fields]) for passport in each_passport])


# part 2: add more validation to the rules
# byr (Birth Year) - four digits; at least 1920 and at most 2002.
# iyr (Issue Year) - four digits; at least 2010 and at most 2020.
# eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
# hgt (Height) - a number followed by either cm or in:
#   If cm, the number must be at least 150 and at most 193.
# If in, the number must be at least 59 and at most 76.
# hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
# ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
# pid (Passport ID) - a nine-digit number, including leading zeroes.
# cid (Country ID) - ignored, missing or not.

fields = ["byr:(19[2-9][0-9]|200[0-2])", 
         "iyr:(201[0-9]|2020)", 
         "eyr:(202[0-9]|2030)",
         "hgt:(1[5-8][0-9]|19[0-3])cm|hgt:(59|6[0-9]|7[0-6])in", 
         "hcl:#[0-9|a-f]{6}(?!\w)", 
         "ecl:(amb|blu|brn|gry|grn|hzl|oth)", 
         "pid:[0-9]{9}(?!\w)"]

sum([all([re.search(pattern, passport) for pattern in fields]) for passport in each_passport])

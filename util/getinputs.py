#!/usr/bin/env python3
import requests
import os
from datetime import date
import browser_cookie3
import sys

# USAGE: python3 getinputs.py 25

# Get cookies from the browser
cj = browser_cookie3.chrome()
    
# Get today number of day
day_today = date.today().strftime("%d").lstrip("0")

# If we provide an argument, use it as the desired day. Ex: ./startDay.py 5. Otherwise use day_today
if len(sys.argv) > 1:
    day = int(sys.argv[1])
    if day<0 or day>31 or day>int(day_today):
        exit("Day is not valid")
else:
    day = day_today


print(f"Initializing day {day}")

if not os.path.exists(f"day{day}"):
    try:
        os.mkdir(f"{day}")
    except FileExistsError:
        pass
    os.chdir(f"{day}")
    r = requests.get(f"https://adventofcode.com/2019/day/{day}/input", cookies = cj)
    with open(f"input{day}.txt","w") as f:
        f.write(r.text)

#!/usr/bin/env python3
import requests
import os
from datetime import date
import browser_cookie3
import sys

# USAGE:
# python3 util/getinputs.py <year> <day
# python3 util/getinputs.py 2019 25

# Get cookies from the browser
cj = browser_cookie3.chrome()

# If we provide arguments, read them as the year and day. Otherwise, use today
if len(sys.argv) > 2:
    year = int(sys.argv[1])
    day = int(sys.argv[2])
elif len(sys.argv) > 1:
    year = int(sys.argv[1])
    day = date.today().strftime("%d").lstrip("0")
else:
    year = date.today().year
    day = date.today().strftime("%d").lstrip("0")

print(f"Initializing day {day}")

if not os.path.exists(f"day{day}"):
    try:
        os.mkdir(f"{day}")
    except FileExistsError:
        pass
    os.chdir(f"{day}")
    r = requests.get(f"https://adventofcode.com/{year}/day/{day}/input", cookies = cj)
    with open(f"../{year}/{day}/input{day}.txt","w") as f:
        f.write(r.text)

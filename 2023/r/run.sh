#! /usr/bin/env bash

# get the input and validate if its a day between 1 and 25
read -r -p "Enter the day: " day
if [ "$day" -lt 1 ] || [ "$day" -gt 25 ]
then
    echo "Invalid day"
    exit 1
fi

# run the appropriate day
Rscript "day$day.R"

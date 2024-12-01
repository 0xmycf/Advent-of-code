#! /usr/bin/env bash

INPUT="../input/day1.txt"
# INPUT="./test.txt"

one=$(cut -d " " -f 1 $INPUT | sort)
two=$(cut -d " " -f 4 $INPUT | sort)

readarray -t array1 <<< "$one"
readarray -t array2 <<< "$two"

echo "doing part a (and prepare b)"

declare -A dict

total=0
for idx in "${!array1[@]}"; do
    elem1=${array1[$idx]}
    elem2=${array2[$idx]}
    diff=$((elem1 - elem2))
    if [ "$diff" -le "0" ]; then
        diff=$((-1 * diff)) 
    fi 
    total=$((total + diff))

    count2=${dict["$elem2"]}
    dict["$elem2"]=$((count2 + 1))
done

echo $total

echo "doing part b"

total=0
for idx in "${!array1[@]}"; do
    elem1=${array1[$idx]}
    count="${dict["$elem1"]}"
    total=$((total + count * elem1))
done

echo $total

# with time
# ---------
# First version
# ________________________________________________________
# Executed in    3.70 secs    fish           external
#    usr time    3.41 secs  121.00 micros    3.41 secs
#    sys time    0.05 secs  806.00 micros    0.05 secs
#
# 'readarray' version
# ________________________________________________________
# Executed in    3.65 secs    fish           external
#    usr time    3.37 secs  133.00 micros    3.37 secs
#    sys time    0.04 secs  814.00 micros    0.04 secs
#
# Final version
# ________________________________________________________
# Executed in   91.87 millis    fish           external
#    usr time   66.99 millis  105.00 micros   66.88 millis
#    sys time   35.88 millis  716.00 micros   35.16 millis

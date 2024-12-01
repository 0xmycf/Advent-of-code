#! /usr/bin/env bash

INPUT="../input/day1.txt"
# INPUT="./test.txt"

one=$(cut -d " " -f 1 $INPUT | sort)
two=$(cut -d " " -f 4 $INPUT | sort)

read -r -a array1 <<< "$(echo "$one" | tr '\n' " ")"
read -r -a array2 <<< "$(echo "$two" | tr '\n' " ")"

echo "doing part a"

total=0
for idx in "${!array1[@]}"; do
    elem1=${array1[$idx]}
    elem2=${array2[$idx]}
    diff=$((elem1 - elem2))
    if [ "$diff" -le "0" ]; then
        diff=$((-1 * diff)) 
    fi 
    total=$((total + diff))
done

echo $total

echo "doing part b"

total=0
for idx in "${!array1[@]}"; do
    elem1=${array1[$idx]}
    count=0

    for elem2 in "${array2[@]}"; do
        if [[ "$elem1" = "$elem2"  ]]; then
            count=$((count + 1))
        fi
    done
    total=$((total + count * elem1))
done

echo $total


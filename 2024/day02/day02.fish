#! /usr/bin/env fish


set -l sum 0
while read -l line
    set -l input (string split " " $line)
    set okge 0
    set okle 0
    set -l lastnum $input[1]
    for x in $input[2..]
        set -l diff (math abs $lastnum - $x) 
        if ! test \( $lastnum -le $x \) -a \( $diff -ge 0 \) -a \( $diff -le 3 \)
            set okle 1
        end
        if ! test \( $lastnum -ge $x \) -a \( $diff -ge 0 \) -a \( $diff -le 3 \)
            set okge 1
        end
        if test $lastnum -eq $x
            set okge 1
            set okle 1
        end
        set lastnum $x
    end
    if test (math $okge + $okle) -eq 1
        set sum (math $sum + 1) 
        set_color red; printf "$line\n"; set_color normal
    end
end

printf $sum


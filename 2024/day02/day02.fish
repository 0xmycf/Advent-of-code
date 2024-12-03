#! /usr/bin/env fish

function sign
    if test $argv -lt 0
        printf -1
    end
    if test $argv -gt 0
        printf 1
    end
    if test $argv -eq 0
        printf 0
    end
end

function inrange
    set -l delta (math abs $argv)
    if test $delta -ge 1 -a $delta -le 3
        printf 0
    else
        printf 1
    end
end

function toconsole
    set_color red
    printf "$argv\n"
    set_color normal
end

function testwithout
    for i in (seq 1 (count $argv))
        set tmp $argv
        set -e tmp[$i]
        set -l ok (testline $tmp)
        if test $ok -eq 0
            printf 0
            return
        end
    end
    printf 1
end

function testline
    set -l input $argv

    set -l sprev
    begin 
        set -l fst $input[1]
        set -l snd $input[2]
        set sprev (sign (math $fst - $snd))
    end

    set -l ok 0

    for idx in (seq 1 (math (count $input) - 1)) # ignore the last element

        set this $input[$idx]
        set that $input[(math $idx + 1)]

        set -l diff (math $this - $that)
        set -l s (sign $diff)

        if test \( $diff -eq 0 \) -o (inrange $diff) -ne 0 -o $sprev -ne $s
            set ok 1
        end
       set sprev (sign $diff)
    end

    printf $ok
end

set -l sum 0
set -l sumb 0
while read -l line
    set -l input (string split " " $line)

    set -l ok (testline $input)

    if test $ok -eq 0
        toconsole $line
        set sum (math $sum + 1)
    else
        set ok (testwithout $input)
        if test $ok -eq 0
            toconsole $line
            set sumb (math $sumb + 1)
        end
    end

end

printf "$sum\n"
printf (math $sum + $sumb)



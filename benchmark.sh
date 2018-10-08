#!/bin/bash

# Parse options.
OPTIND=1

output_file=""
lower=0
upper=0

while getopts "o:l:u:" opt; do
    case "$opt" in
        o)  output_file=$OPTARG
            ;;
        l)  lower=$OPTARG
            ;;
        u)  upper=$OPTARG
            ;;
    esac
done

shift $((OPTIND-1))

[ "${1:-}" = "--" ] && shift

# Validate options.
if [ -z "$output_file" ]
then
    echo "benchmark: output file needed"
    exit
fi
if [ "$lower" -ge "$upper" ]
then
    echo "benchmark: lower bound >= upper bound"
    exit
fi

# Run the benchmarks.
for size in `seq $lower $upper`
do
    cmd="eleanor $@ --size $size"
    echo $cmd

    # if the size is smaller than 20, then the run times are short enough that
    # we should probably do averaging over several runs... but if it's bigger,
    # then we can probably just time a single run
    if [ "$size" -le 20 ]
    then
        AVG="0"
        for i in `seq 1 10`
        do
            TIME=$({ gtime -f '%S + %U' stack exec eleanor -- $@ --action Count --size $size > /dev/null ; } 2>&1 | bc -l)
            AVG=$(echo "$AVG + $TIME" | bc -l)
        done
        AVG=$(echo "$AVG / 10.0" | bc -l)
    else
        AVG=$({ gtime -f '%S + %U' stack exec eleanor -- $@ --action Count --size $size > /dev/null ; } 2>&1 | bc -l)
    fi
    echo "$@,$size,$AVG" >> $output_file
done

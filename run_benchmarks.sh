#!/bin/bash

for i in `seq 26 30`
do
    ./benchmark.sh -o benchmarks/bench.Untyped.Neat.log     -l $i -u $i -- --system Untyped --strategy Neat
    ./benchmark.sh -o benchmarks/bench.SimplyTyped.Feat.log -l $i -u $i -- --system SimplyTyped --strategy Feat
    ./benchmark.sh -o benchmarks/bench.Linear.Feat.log      -l $i -u $i -- --system Linear      --strategy Feat
done

./benchmark.sh -o benchmarks/bench.Untyped.Feat.log -l 30 -u 30 -- --system Untyped --strategy Feat


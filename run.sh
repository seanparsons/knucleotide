#!/usr/bin/env sh
set -e
stack build --executable-profiling

echo "Old"
#stack exec knucleotide -- --old +RTS -p -N4 -K2048M < knucleotide-input-small.txt
stack exec knucleotide -- --old +RTS -p -N4 -K2048M < input250000.txt
#stack exec knucleotide -- --old +RTS -p -N4 -K2048M < input25000000.txt
mv knucleotide.prof knucleotide-old.prof

echo "New"
#stack exec knucleotide -- +RTS -p -N4 -K2048M < knucleotide-input-small.txt
stack exec knucleotide -- +RTS -p -N4 -K2048M < input250000.txt
#stack exec knucleotide -- +RTS -p -N4 -K2048M < input25000000.txt
mv knucleotide.prof knucleotide-new.prof

#!/bin/sh
#
# syntax "proto3" is not supported so we have to use VEGY ugly sed
# hacks
#
# Script is expected to be run from main directory
set -e
[ -f krpc-hs.cabal ] || { echo "Not in root of project"; exit 1; }

# Run
VER=${1:-0.4.3}
PROTO=KRPC/"$VER"/krpc.proto
TEMP=KRPC/krpc.proto
sed -re 's/^  ([A-Z]|string|bytes|uint|int|float|double|bool)/  optional \1/' \
    $PROTO > $TEMP

rm -rfv src/PB
hprotoc --prefix=PB --haskell_out=src $TEMP
rm $TEMP

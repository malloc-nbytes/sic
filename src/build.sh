#!/bin/bash

set -xe

if [ "$1" == "clean" ];
then
    rm -rf *.hi *.o sic
else
    ghc -o sic Main.hs
fi

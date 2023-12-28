#!/bin/bash

set -xe

if [ "$1" == "clean" ];
then
    rm -rf *.hi *.o main
else
    ghc -o main Main.hs
fi

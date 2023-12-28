#!/bin/bash

set -xe

if [ "$1" == "clean" ];
then
    rm -rf *.hi *.o tfc
else
    ghc -o tfc Main.hs
fi

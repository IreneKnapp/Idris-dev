#!/bin/sh -ex

find . -name "*.ibc" | xargs rm -f
find . -name "*.o" | xargs rm -f
find . -name "*.a" | xargs rm -f

rm -rf dist

cabal update
cabal install

make test

#!/bin/bash

source scripts/common.sh

mute 5 scripts/setup.sh &&
mute 3 scripts/build.sh && (
 cabal test $@
 exit $PIPESTATUS
)

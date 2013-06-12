#!/bin/bash

cabal test $@ | grep "^Test suite " | grep -v ": RUNNING...$"
exit $PIPESTATUS

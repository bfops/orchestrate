#!/bin/bash

source scripts/common.sh

mute 5 scripts/setup.sh && (
  cabal build $@ 2>&1 |
  grep -v "^\(Loading package\|You are using a new version of LLVM that hasn't been tested yet!$\|We will try though...$\)"
  exit $PIPESTATUS
)

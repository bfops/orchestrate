#!/bin/bash

function mute() {
  MAX=$1
  shift 1
  OUT=`$@ 2>&1`
  RET=$?
  if [ $RET -ne 0 ] || [ `echo "$OUT" | wc -l` -gt $MAX ]; then
    echo "$OUT"
  fi
  return $RET
}

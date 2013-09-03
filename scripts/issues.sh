#!/bin/bash

cd meristem

find -type f | while read file; do
  echo "$file": `head -1 "$file"`
done

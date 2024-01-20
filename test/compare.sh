#!/bin/bash

chibicc2=$1
chibicc3=$2

diff_lines=$(cmp -l $chibicc2 $chibicc3 | wc -l)

if [ $diff_lines -le 1 ]; then
  echo OK
else
  echo NG
  exit 1
fi

#!/bin/bash

chibicc2=$1
chibicc3=$2

diff_lines=$(cmp -l $chibicc2 $chibicc3 | wc -l)

if [ $diff_lines -gt 1 ]; then
  echo NG
  exit 1
fi

echo OK

#!/bin/bash

chibicc2=$1
chibicc3=$2

cmp -l $chibicc2 $chibicc3
if [ $? -ne 0 ]; then
  echo NG
  exit 1
fi

echo OK

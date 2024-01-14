#!/bin/bash

clang -v

chibicc=$1

tmp=`mktemp -d /tmp/chibicc-test-XXXXXX`
trap "rm -rf $tmp" INT TERM HUP EXIT
echo > $tmp/empty.c

check() {
  if [ $? -eq 0 ]; then
    echo "testing $1 ... passed"
  else
    echo "testing $1 ... failed"
    exit 1
  fi
}

# -o
rm -f $tmp/out
$chibicc -o $tmp/out $tmp/empty.c
[ -f $tmp/out ]
check "-o"

# --help
$chibicc --help 2>&1 | grep -q chibicc
check "--help"

# -S
echo 'int main() {}' | $chibicc -S -o - - | grep -q '_main:'
check "-S"

rm -f $tmp/out.o $tmp/out.s
echo 'int main() {}' > $tmp/out.c
(cd $tmp; $chibicc out.c)
[ -f $tmp/out.o ]
check "default output file"

(cd $tmp; $chibicc -S out.c)
[ -f $tmp/out.s ]
check "default output file"


rm -f $tmp/foo.o $tmp/bar.o
echo 'int x;' > $tmp/foo.c
echo 'int y;' > $tmp/bar.c
(cd $tmp; $chibicc foo.c bar.c)
[ -f $tmp/foo.o ] && [ -f $tmp/bar.o ]
check "multiple input files"

rm -f $tmp/foo.s $tmp/bar.s
echo 'int x;' > $tmp/foo.c
echo 'int y;' > $tmp/bar.c
(cd $tmp; $chibicc -S foo.c bar.c)
[ -f $tmp/foo.s ] && [ -f $tmp/bar.s ]
check "multiple input files"

echo OK

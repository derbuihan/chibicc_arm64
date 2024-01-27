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
$chibicc -c -o $tmp/out $tmp/empty.c
[ -f $tmp/out ]
check "-o"

# --help
$chibicc --help 2>&1 | grep -q chibicc
check "--help"

# -S
echo 'int main() {}' | $chibicc -S -o - - | grep -q '_main:'
check "-S"

# Default output file
rm -f $tmp/out.o $tmp/out.s
echo 'int main() {}' > $tmp/out.c
(cd $tmp; $chibicc -c out.c)
[ -f $tmp/out.o ]
check "default output file"

(cd $tmp; $chibicc -c -S out.c)
[ -f $tmp/out.s ]
check "default output file"

rm -f $tmp/foo.o $tmp/bar.o
echo 'int x;' > $tmp/foo.c
echo 'int y;' > $tmp/bar.c
(cd $tmp; $chibicc -c foo.c bar.c)
[ -f $tmp/foo.o ] && [ -f $tmp/bar.o ]
check "multiple input files"

rm -f $tmp/foo.s $tmp/bar.s
echo 'int x;' > $tmp/foo.c
echo 'int y;' > $tmp/bar.c
(cd $tmp; $chibicc -c -S foo.c bar.c)
[ -f $tmp/foo.s ] && [ -f $tmp/bar.s ]
check "multiple input files"

# Named output file
rm -f $tmp/out.o
echo 'int main() {}' > $tmp/out.c
(cd $tmp; $chibicc -c -o out.o out.c)
[ -f $tmp/out.o ]
check "named output file"

rm -f $tmp/out.s
echo 'int main() {}' > $tmp/out.c
(cd $tmp; $chibicc -c -S -o out.s out.c)
[ -f $tmp/out.s ]
check "named output file"

# assembly
rm -f $tmp/out.o
cat <<EOF > $tmp/out.s
.text
.global _main
_main:
  mov x0, 42
  ret
EOF
$chibicc $tmp/out.s -o $tmp/out.o
[ -f $tmp/out.o ]
check "assembly"

rm -f $tmp/out.o
cat <<EOF > $tmp/out.s
.text
.global _main
_main:
  mov x0, 42
  ret
EOF
(cd $tmp; $chibicc out.s)
[ -f $tmp/out.o ]
check "assembly"

# Run linker
rm -f $tmp/foo
echo 'int main() { return 0; }' | $chibicc -o $tmp/foo -
$tmp/foo
check "linker"

rm -f $tmp/foo
echo 'int bar(); int main() { return bar(); }' > $tmp/foo.c
echo 'int bar() { return 42; }' > $tmp/bar.c
$chibicc -o $tmp/foo $tmp/foo.c $tmp/bar.c
$tmp/foo
[ $? -eq 42 ]
check "linker"

rm -f $tmp/a.out
echo 'int main() {}' > $tmp/foo.c
(cd $tmp; $chibicc foo.c)
[ -f $tmp/a.out ]
check "a.out"

# -E
echo "foo" > $tmp/out
echo "#include \"$tmp/out\"" | $chibicc -E - | grep -q "foo"
check "-E"

echo "bar" > $tmp/out1
echo "#include \"$tmp/out1\"" | $chibicc -E -o $tmp/out2 -
cat $tmp/out2 | grep -q "bar"
check "-E and -o"

# -I
mkdir $tmp/dir
echo "foo" > $tmp/dir/i-option-test
echo "#include \"i-option-test\"" | $chibicc -I$tmp/dir -E - | grep -q "foo"
check "-I"

echo OK

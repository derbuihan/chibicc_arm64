chibicc_arm64
===

C compiler on M1 Mac.

## Build

```bash
$ cmake -S . -B build
$ cmake --build build
```

Generated files:

* `build/chibicc` : C compiler
* `build/chibicc2` : C compiler (stage 2)
* `build/chibicc3` : C compiler (stage 3)

## Run

```bash
$ ./build/chibicc -S -o tmp.s tmp.c
$ clang -o tmp tmp.s
$ ./tmp
```

Usage:

* `-o <file>` : Output file name
* `-S` : Generate assembly code
* `-c` : Generate object file
* `-E` : Generate preprocessed code
* `-I <dir>` : Add include path

## Test

```bash
$ cd build
$ ctest
```

## Reference

* https://www.sigbus.info/compilerbook
* https://github.com/rui314/chibicc

chibicc_arm64
===

C compiler on M1 Mac.

## Prerequisites

Before building the compiler, ensure you have the following installed on your M1 Mac:

* CMake
* Xcode
    * Xcode is specifically required for M1 Mac users. Install Xcode from the App Store or by
      running `xcode-select --install` in your terminal.

Note: This compiler is designed to work exclusively on M1 Macs and may not function correctly on other platforms.

## Build

```bash
$ cmake -S . -B build
$ cmake --build build
```

Generated files:

* `build/chibicc` : C compiler
* `build/chibicc2` : C compiler (stage 2)
* `build/chibicc3` : C compiler (stage 3)

Stage 3 compiler (chibicc3) is verified to match the stage 2 compiler (chibicc2) in output, ensuring the correctness of
the bootstrapping process.

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

Running the tests will help ensure that your build is functioning correctly and that any changes made have not
introduced regressions.

## Todo

- [ ] function that take 9 or more arguments
- [ ] preprocesser : `__attribute__`, `__asm`, `__has_include`
- [ ] designated initializer

## Reference

* For a comprehensive guide on writing a C compiler, visit: https://www.sigbus.info/compilerbook
* Check out the original chibicc compiler by Rui Ueyama at: https://github.com/rui314/chibicc

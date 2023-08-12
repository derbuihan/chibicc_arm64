cmake_minimum_required(VERSION 3.26)
project(chibicc C)

set(CMAKE_C_STANDARD 17)

add_executable(chibicc main.c type.c tokenize.c parse.c codegen.c)

enable_testing()
add_test(test bash ${CMAKE_CURRENT_SOURCE_DIR}/test.sh)

cmake_minimum_required(VERSION 3.14)
project(chibicc C)
set(CMAKE_C_STANDARD 17)

# build chibicc
file(GLOB SRCS *.c)
add_executable(chibicc ${SRCS})

# build tests
enable_testing()
add_test(NAME driver COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/test/driver.sh)
file(GLOB TEST_SRCS test/*.c)
foreach (TEST_SRC ${TEST_SRCS})
    get_filename_component(TEST_NAME ${TEST_SRC} NAME_WE)
    add_custom_target(
            ${TEST_NAME}.c ALL
            COMMAND clang -o ${TEST_NAME}.c -P -E -C ${TEST_SRC}
            DEPENDS chibicc
    )
    add_custom_target(
            ${TEST_NAME}.s ALL
            COMMAND ./chibicc -o ${TEST_NAME}.s ${TEST_NAME}.c
            DEPENDS ${TEST_NAME}.c
    )
    add_custom_target(
            ${TEST_NAME}.exe ALL
            COMMAND clang -o ${TEST_NAME}.exe ${TEST_NAME}.s -x c ${CMAKE_CURRENT_SOURCE_DIR}/test/common
            DEPENDS ${TEST_NAME}.s
    )
    add_test(NAME ${TEST_NAME} COMMAND ${TEST_NAME}.exe)
endforeach ()

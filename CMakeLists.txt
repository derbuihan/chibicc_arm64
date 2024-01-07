cmake_minimum_required(VERSION 3.14)
project(chibicc C)
set(CMAKE_C_STANDARD 17)

# build chibicc
file(GLOB SRCS *.c)
add_executable(chibicc ${SRCS})


# build stage2
set(STAGE2_OUTPUT_FILES "")
set(STAGE2_OUTPUT_DIR "${CMAKE_CURRENT_BINARY_DIR}/stage2")
file(MAKE_DIRECTORY ${STAGE2_OUTPUT_DIR})

foreach (SRC ${SRCS})
    get_filename_component(SRC_NAME ${SRC} NAME_WE)
    set(STAGE2_OUTPUT_C_FILE "${STAGE2_OUTPUT_DIR}/${SRC_NAME}.c")
    set(STAGE2_OUTPUT_S_FILE "${STAGE2_OUTPUT_DIR}/${SRC_NAME}.s")
    add_custom_command(
            OUTPUT ${STAGE2_OUTPUT_C_FILE}
            COMMAND python3 ${CMAKE_CURRENT_SOURCE_DIR}/self.py ${CMAKE_CURRENT_SOURCE_DIR}/chibicc.h ${SRC} > ${STAGE2_OUTPUT_C_FILE}
            DEPENDS chibicc
    )
    list(APPEND STAGE2_OUTPUT_FILES ${STAGE2_OUTPUT_C_FILE})
endforeach ()

add_custom_target(stage2 DEPENDS ${STAGE2_OUTPUT_FILES})

# build tests
enable_testing()
file(GLOB TEST_SRCS test/*.c)

set(TEST_OUTPUT_DIR "${CMAKE_CURRENT_BINARY_DIR}/test")
file(MAKE_DIRECTORY ${TEST_OUTPUT_DIR})

add_test(NAME driver COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/test/driver.sh)
foreach (TEST_SRC ${TEST_SRCS})
    get_filename_component(TEST_NAME ${TEST_SRC} NAME_WE)
    set(TEST_OUTPUT_C_FILE "${TEST_OUTPUT_DIR}/${TEST_NAME}.c")
    set(TEST_OUTPUT_S_FILE "${TEST_OUTPUT_DIR}/${TEST_NAME}.s")
    set(TEST_OUTPUT_EXE_FILE "${TEST_OUTPUT_DIR}/${TEST_NAME}.exe")
    add_custom_target(
            ${TEST_NAME} ALL
            COMMAND clang -o ${TEST_OUTPUT_C_FILE} -P -E -C ${TEST_SRC}
            COMMAND ./chibicc -o ${TEST_OUTPUT_S_FILE} ${TEST_OUTPUT_C_FILE}
            COMMAND clang -o ${TEST_OUTPUT_EXE_FILE} ${TEST_OUTPUT_S_FILE} -x c ${CMAKE_CURRENT_SOURCE_DIR}/test/common
            DEPENDS chibicc
    )
    add_test(NAME ${TEST_NAME} COMMAND ${TEST_OUTPUT_EXE_FILE})
endforeach ()

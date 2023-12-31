cmake_minimum_required(VERSION 3.14)
project(chibicc C)
set(CMAKE_C_STANDARD 17)

# build chibicc
file(GLOB SRCS *.c)
add_executable(chibicc ${SRCS})

# build stage2
set(STAGE2_OUTPUT_DIR "${CMAKE_CURRENT_BINARY_DIR}/stage2")
file(MAKE_DIRECTORY ${STAGE2_OUTPUT_DIR})

set(STAGE2_OUTPUT_FILES "")
foreach (SRC ${SRCS})
    get_filename_component(SRC_NAME ${SRC} NAME_WE)
    set(STAGE2_OUTPUT_C_FILE "${STAGE2_OUTPUT_DIR}/${SRC_NAME}.c")
    set(STAGE2_OUTPUT_S_FILE "${STAGE2_OUTPUT_DIR}/${SRC_NAME}.s")
    set(STAGE2_OUTPUT_OBJ_FILE "${STAGE2_OUTPUT_DIR}/${SRC_NAME}.o")
    add_custom_command(
            OUTPUT ${STAGE2_OUTPUT_OBJ_FILE}
            COMMAND python3 ${CMAKE_CURRENT_SOURCE_DIR}/self.py ${CMAKE_CURRENT_SOURCE_DIR}/chibicc.h ${SRC} > ${STAGE2_OUTPUT_C_FILE}
            COMMAND ./chibicc -o ${STAGE2_OUTPUT_S_FILE} ${STAGE2_OUTPUT_C_FILE}
            COMMAND clang -c -o ${STAGE2_OUTPUT_OBJ_FILE} ${STAGE2_OUTPUT_S_FILE}
            DEPENDS chibicc ${CMAKE_CURRENT_SOURCE_DIR}/chibicc.h ${SRC} ${CMAKE_CURRENT_SOURCE_DIR}/self.py
    )
    list(APPEND STAGE2_OUTPUT_FILES ${STAGE2_OUTPUT_OBJ_FILE})
endforeach ()

add_custom_target(stage2 ALL DEPENDS ${STAGE2_OUTPUT_FILES})
add_executable(chibicc2 ${STAGE2_OUTPUT_FILES})
set_target_properties(chibicc2 PROPERTIES LINKER_LANGUAGE C)
add_dependencies(chibicc2 stage2)

# build tests
enable_testing()
file(GLOB TEST_SRCS test/*.c)

set(TEST_OUTPUT_DIR "${CMAKE_CURRENT_BINARY_DIR}/test")
file(MAKE_DIRECTORY ${TEST_OUTPUT_DIR})

add_test(
        NAME driver
        COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/test/driver.sh ${CMAKE_CURRENT_BINARY_DIR}/chibicc
)
foreach (TEST_SRC ${TEST_SRCS})
    get_filename_component(TEST_NAME ${TEST_SRC} NAME_WE)
    set(TEST_OUTPUT_S_FILE "${TEST_OUTPUT_DIR}/${TEST_NAME}.s")
    set(TEST_OUTPUT_EXE_FILE "${TEST_OUTPUT_DIR}/${TEST_NAME}.exe")
    add_custom_target(
            ${TEST_NAME} ALL
            COMMAND clang -o- -E -P -C ${TEST_SRC} | ./chibicc -o ${TEST_OUTPUT_S_FILE} -
            COMMAND clang -o ${TEST_OUTPUT_EXE_FILE} ${TEST_OUTPUT_S_FILE} -x c ${CMAKE_CURRENT_SOURCE_DIR}/test/common
            DEPENDS chibicc ${TEST_SRC}
    )
    add_test(NAME ${TEST_NAME} COMMAND ${TEST_OUTPUT_EXE_FILE})
endforeach ()

# build stage2 tests
add_test(
        NAME stage2_driver
        COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/test/driver.sh ${CMAKE_CURRENT_BINARY_DIR}/chibicc2
)

#set(STAGE2_TEST_OUTPUT_DIR "${CMAKE_CURRENT_BINARY_DIR}/stage2/test")
#file(MAKE_DIRECTORY ${STAGE2_TEST_OUTPUT_DIR})
#foreach (TEST_SRC ${TEST_SRCS})
#    get_filename_component(TEST_NAME ${TEST_SRC} NAME_WE)
#    set(STAGE2_TEST_OUTPUT_S_FILE "${STAGE2_TEST_OUTPUT_DIR}/${TEST_NAME}.s")
#    set(STAGE2_TEST_OUTPUT_EXE_FILE "${STAGE2_TEST_OUTPUT_DIR}/${TEST_NAME}.exe")
#    add_custom_target(
#            stage2_${TEST_NAME} ALL
#            COMMAND clang -o- -E -P -C ${TEST_SRC} | ./chibicc2 -o ${STAGE2_TEST_OUTPUT_S_FILE} -
#            COMMAND clang -o ${STAGE2_TEST_OUTPUT_EXE_FILE} ${STAGE2_TEST_OUTPUT_S_FILE} -x c ${CMAKE_CURRENT_SOURCE_DIR}/test/common
#            DEPENDS chibicc2
#    )
#    add_test(NAME stage2_${TEST_NAME} COMMAND ${STAGE2_TEST_OUTPUT_EXE_FILE})
#endforeach ()

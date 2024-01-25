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
            COMMAND ./chibicc -c -S -o ${STAGE2_OUTPUT_S_FILE} ${STAGE2_OUTPUT_C_FILE}
            COMMAND ./chibicc -o ${STAGE2_OUTPUT_OBJ_FILE} ${STAGE2_OUTPUT_S_FILE}
            DEPENDS chibicc ${CMAKE_CURRENT_SOURCE_DIR}/chibicc.h ${SRC} ${CMAKE_CURRENT_SOURCE_DIR}/self.py
    )
    list(APPEND STAGE2_OUTPUT_FILES ${STAGE2_OUTPUT_OBJ_FILE})
endforeach ()

add_custom_target(stage2 ALL DEPENDS ${STAGE2_OUTPUT_FILES})
add_executable(chibicc2 ${STAGE2_OUTPUT_FILES})
set_target_properties(chibicc2 PROPERTIES LINKER_LANGUAGE C)
add_dependencies(chibicc2 stage2)

# build stage3
set(STAGE3_OUTPUT_DIR "${CMAKE_CURRENT_BINARY_DIR}/stage3")
file(MAKE_DIRECTORY ${STAGE3_OUTPUT_DIR})

set(STAGE3_OUTPUT_FILES "")
foreach (SRC ${SRCS})
    get_filename_component(SRC_NAME ${SRC} NAME_WE)
    set(STAGE3_OUTPUT_C_FILE "${STAGE3_OUTPUT_DIR}/${SRC_NAME}.c")
    set(STAGE3_OUTPUT_S_FILE "${STAGE3_OUTPUT_DIR}/${SRC_NAME}.s")
    set(STAGE3_OUTPUT_OBJ_FILE "${STAGE3_OUTPUT_DIR}/${SRC_NAME}.o")
    add_custom_command(
            OUTPUT ${STAGE3_OUTPUT_OBJ_FILE}
            COMMAND python3 ${CMAKE_CURRENT_SOURCE_DIR}/self.py ${CMAKE_CURRENT_SOURCE_DIR}/chibicc.h ${SRC} > ${STAGE3_OUTPUT_C_FILE}
            COMMAND ./chibicc2 -c -S -o ${STAGE3_OUTPUT_S_FILE} ${STAGE3_OUTPUT_C_FILE}
            COMMAND ./chibicc2 -o ${STAGE2_OUTPUT_OBJ_FILE} ${STAGE2_OUTPUT_S_FILE}
            DEPENDS chibicc2 ${CMAKE_CURRENT_SOURCE_DIR}/chibicc.h ${SRC} ${CMAKE_CURRENT_SOURCE_DIR}/self.py
    )
    list(APPEND STAGE3_OUTPUT_FILES ${STAGE3_OUTPUT_OBJ_FILE})
endforeach ()

add_custom_target(stage3 ALL DEPENDS ${STAGE3_OUTPUT_FILES})
add_executable(chibicc3 ${STAGE3_OUTPUT_FILES})
set_target_properties(chibicc3 PROPERTIES LINKER_LANGUAGE C)
add_dependencies(chibicc3 stage3)

# build tests
enable_testing()
file(GLOB TEST_SRCS test/*.c)

set(TEST_OUTPUT_DIR "${CMAKE_CURRENT_BINARY_DIR}/test")
file(MAKE_DIRECTORY ${TEST_OUTPUT_DIR})

add_test(NAME driver COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/test/driver.sh ${CMAKE_CURRENT_BINARY_DIR}/chibicc)
foreach (TEST_SRC ${TEST_SRCS})
    get_filename_component(TEST_NAME ${TEST_SRC} NAME_WE)
    set(TEST_OUTPUT_S_FILE "${TEST_OUTPUT_DIR}/${TEST_NAME}.s")
    set(TEST_OUTPUT_OBJ_FILE "${TEST_OUTPUT_DIR}/${TEST_NAME}.o")
    set(TEST_OUTPUT_EXE_FILE "${TEST_OUTPUT_DIR}/${TEST_NAME}.exe")
    add_custom_target(
            ${TEST_NAME} ALL
            COMMAND ./chibicc -c -S -o ${TEST_OUTPUT_S_FILE} ${TEST_SRC}
            COMMAND ./chibicc -o ${TEST_OUTPUT_OBJ_FILE} ${TEST_OUTPUT_S_FILE}
            COMMAND clang -o ${TEST_OUTPUT_EXE_FILE} ${TEST_OUTPUT_OBJ_FILE} -x c ${CMAKE_CURRENT_SOURCE_DIR}/test/common
            DEPENDS chibicc ${TEST_SRC}
    )
    add_test(NAME ${TEST_NAME} COMMAND ${TEST_OUTPUT_EXE_FILE})
endforeach ()

# build stage2 tests
set(STAGE2_TEST_OUTPUT_DIR "${CMAKE_CURRENT_BINARY_DIR}/stage2/test")
file(MAKE_DIRECTORY ${STAGE2_TEST_OUTPUT_DIR})

add_test(NAME stage2_driver COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/test/driver.sh ${CMAKE_CURRENT_BINARY_DIR}/chibicc2)
foreach (TEST_SRC ${TEST_SRCS})
    get_filename_component(TEST_NAME ${TEST_SRC} NAME_WE)
    set(STAGE2_TEST_OUTPUT_S_FILE "${STAGE2_TEST_OUTPUT_DIR}/${TEST_NAME}.s")
    set(STAGE2_TEST_OUTPUT_OBJ_FILE "${STAGE2_TEST_OUTPUT_DIR}/${TEST_NAME}.o")
    set(STAGE2_TEST_OUTPUT_EXE_FILE "${STAGE2_TEST_OUTPUT_DIR}/${TEST_NAME}.exe")
    add_custom_target(
            stage2_${TEST_NAME} ALL
            COMMAND ./chibicc2 -c -S -o ${STAGE2_TEST_OUTPUT_S_FILE} ${TEST_SRC}
            COMMAND ./chibicc2 -o ${STAGE2_TEST_OUTPUT_OBJ_FILE} ${STAGE2_TEST_OUTPUT_S_FILE}
            COMMAND clang -o ${STAGE2_TEST_OUTPUT_EXE_FILE} ${STAGE2_TEST_OUTPUT_OBJ_FILE} -x c ${CMAKE_CURRENT_SOURCE_DIR}/test/common
            DEPENDS chibicc2 ${TEST_SRC}
    )
    add_test(NAME stage2_${TEST_NAME} COMMAND ${STAGE2_TEST_OUTPUT_EXE_FILE})
endforeach ()

# compare stage2 and stage3
add_test(
        NAME compare_stage2_stage3
        COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/test/compare.sh ${CMAKE_CURRENT_BINARY_DIR}/chibicc2 ${CMAKE_CURRENT_BINARY_DIR}/chibicc3
)

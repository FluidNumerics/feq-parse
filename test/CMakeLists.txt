CMAKE_MINIMUM_REQUIRED(VERSION 3.0.2)

set(CMAKE_VERBOSE_MAKEFILE ON)

if ("${CMAKE_SOURCE_DIR}" STREQUAL "${CMAKE_CURRENT_SOURCE_DIR}")
  project(test-feqparse)
endif()
set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/include)
set(make_env FEQPARSE_HOME=${CMAKE_BINARY_DIR} FEQPARSE_COMPILER=${FEQPARSE_COMPILER})

enable_testing()
function (mangle_fortran_name CNAME FNAME)
    set (TMP)
    if (WIN32)
        string (TOUPPER "${FNAME}" TMP)
    else ()
        string (TOLOWER "${FNAME}_" TMP)
    endif ()
    set (${CNAME} ${TMP} PARENT_SCOPE)
endfunction ()
function (mangle_fortran_filename_list MANGLED)
    set (TMP)
    foreach (TFILE ${ARGN})
        string (REGEX REPLACE ".f90$" "" TESTNAME ${TFILE})
        mangle_fortran_name (C_TESTNAME ${TESTNAME})
        list (APPEND TMP ${C_TESTNAME})
    endforeach ()
    set (${MANGLED} ${TMP} PARENT_SCOPE)
endfunction()

function (add_fortran_test_executable TARGET)
    set (TEST_FILES ${ARGN})
    mangle_fortran_filename_list (TEST_FILES_MANGLED ${TEST_FILES})

    create_test_sourcelist (_ main.c ${TEST_FILES_MANGLED})

    add_library (${TARGET}_fortran ${TEST_FILES})
    target_include_directories(${TARGET}_fortran PUBLIC ${CMAKE_BINARY_DIR}/include)
    add_executable (${TARGET} main.c)
    target_link_libraries (${TARGET} ${TARGET}_fortran)
    target_include_directories (${TARGET} PUBLIC ${TARGET}_fortran)
    target_link_libraries (${TARGET} ${CMAKE_BINARY_DIR}/src/libfeqparse-static.a)
    target_include_directories(${TARGET} PUBLIC ${CMAKE_BINARY_DIR}/include)

    set (INDEX 0)
    list (LENGTH TEST_FILES LEN)
    while (${LEN} GREATER ${INDEX})
        list (GET TEST_FILES ${INDEX} TEST_FILE)
        list (GET TEST_FILES_MANGLED ${INDEX} TEST_FILE_MANGLED)
        add_test (
            NAME ${TEST_FILE}
            COMMAND $<TARGET_FILE:${TARGET}> ${TEST_FILE_MANGLED})
        math (EXPR INDEX "${INDEX} + 1")
    endwhile ()
endfunction ()

add_fortran_test_executable (
    testsuite
    "gaussian3d_sfp32.f90"
    "gaussian3d_sfp64.f90"
    "tanh_with_t.f90"
    "linear_r1fp32.f90"
    "linear_r1fp64.f90"
    "cos_r1fp64.f90"
    "sin_r1fp64.f90"
    "tan_r1fp64.f90"
    "cos_r1fp32.f90"
    "sin_r1fp32.f90"
    "tan_r1fp32.f90")

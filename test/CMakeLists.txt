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
    "abs_r1fp32.f90"
    "abs_r1fp64.f90"
    "abs_r2fp32.f90"
    "abs_r2fp64.f90"
    "abs_r3fp32.f90"
    "abs_r3fp64.f90"
    "abs_r4fp32.f90"
    "abs_r4fp64.f90"
    "abs_sfp32.f90"
    "abs_sfp64.f90"
    "acos_r1fp32.f90"
    "acos_r1fp64.f90"
    "acos_r2fp32.f90"
    "acos_r2fp64.f90"
    "acos_r3fp32.f90"
    "acos_r3fp64.f90"
    "acos_r4fp32.f90"
    "acos_r4fp64.f90"
    "acos_sfp32.f90"
    "acos_sfp64.f90"
    "asin_r1fp32.f90"
    "asin_r1fp64.f90"
    "asin_r2fp32.f90"
    "asin_r2fp64.f90"
    "asin_r3fp32.f90"
    "asin_r3fp64.f90"
    "asin_r4fp32.f90"
    "asin_r4fp64.f90"
    "asin_sfp32.f90"
    "asin_sfp64.f90"
    "atan_r1fp32.f90"
    "atan_r1fp64.f90"
    "atan_r2fp32.f90"
    "atan_r2fp64.f90"
    "atan_r3fp32.f90"
    "atan_r3fp64.f90"
    "atan_r4fp32.f90"
    "atan_r4fp64.f90"
    "atan_sfp32.f90"
    "atan_sfp64.f90"
    "cos_r1fp32.f90"
    "cos_r1fp64.f90"
    "cos_r2fp32.f90"
    "cos_r2fp64.f90"
    "cos_r3fp32.f90"
    "cos_r3fp64.f90"
    "cos_r4fp32.f90"
    "cos_r4fp64.f90"
    "cos_sfp32.f90"
    "cos_sfp64.f90"
    "division_r1fp32.f90"
    "division_r1fp64.f90"
    "division_r2fp32.f90"
    "division_r2fp64.f90"
    "division_r3fp32.f90"
    "division_r3fp64.f90"
    "division_r4fp32.f90"
    "division_r4fp64.f90"
    "division_sfp32.f90"
    "division_sfp64.f90"
    "gaussian3d_r1fp32.f90"
    "gaussian3d_r1fp64.f90"
    "gaussian3d_r2fp32.f90"
    "gaussian3d_r2fp64.f90"
    "gaussian3d_r3fp32.f90"
    "gaussian3d_r3fp64.f90"
    "gaussian3d_r4fp32.f90"
    "gaussian3d_r4fp64.f90"
    "gaussian3d_sfp32.f90"
    "gaussian3d_sfp64.f90"
    "linear_r1fp32.f90"
    "linear_r1fp64.f90"
    "linear_r2fp32.f90"
    "linear_r2fp64.f90"
    "linear_r3fp32.f90"
    "linear_r3fp64.f90"
    "linear_r4fp32.f90"
    "linear_r4fp64.f90"
    "log10_r1fp32.f90"
    "log10_r1fp64.f90"
    "log10_r2fp32.f90"
    "log10_r2fp64.f90"
    "log10_r3fp32.f90"
    "log10_r3fp64.f90"
    "log10_r4fp32.f90"
    "log10_r4fp64.f90"
    "log10_sfp32.f90"
    "log10_sfp64.f90"
    "log_r1fp32.f90"
    "log_r1fp64.f90"
    "log_r2fp32.f90"
    "log_r2fp64.f90"
    "log_r3fp32.f90"
    "log_r3fp64.f90"
    "log_r4fp32.f90"
    "log_r4fp64.f90"
    "log_sfp32.f90"
    "log_sfp64.f90"
    "monadic_r1fp32.f90"
    "monadic_r1fp64.f90"
    "monadic_r2fp32.f90"
    "monadic_r2fp64.f90"
    "monadic_r3fp32.f90"
    "monadic_r3fp64.f90"
    "monadic_r4fp32.f90"
    "monadic_r4fp64.f90"
    "monadic_sfp32.f90"
    "monadic_sfp64.f90"
    "random_r1fp32.f90"
    "random_r1fp64.f90"
    "random_r2fp32.f90"
    "random_r2fp64.f90"
    "random_r3fp32.f90"
    "random_r3fp64.f90"
    "random_r4fp32.f90"
    "random_r4fp64.f90"
    "random_sfp32.f90"
    "random_sfp64.f90"
    "sech_r1fp32.f90"
    "sech_r1fp64.f90"
    "sech_r2fp32.f90"
    "sech_r2fp64.f90"
    "sech_r3fp32.f90"
    "sech_r3fp64.f90"
    "sech_r4fp32.f90"
    "sech_r4fp64.f90"
    "sech_sfp32.f90"
    "sech_sfp64.f90"
    "sin_r1fp32.f90"
    "sin_r1fp64.f90"
    "sin_r2fp32.f90"
    "sin_r2fp64.f90"
    "sin_r3fp32.f90"
    "sin_r3fp64.f90"
    "sin_r4fp32.f90"
    "sin_r4fp64.f90"
    "sin_sfp32.f90"
    "sin_sfp64.f90"
    "sqrt_r1fp32.f90"
    "sqrt_r1fp64.f90"
    "sqrt_r2fp32.f90"
    "sqrt_r2fp64.f90"
    "sqrt_r3fp32.f90"
    "sqrt_r3fp64.f90"
    "sqrt_r4fp32.f90"
    "sqrt_r4fp64.f90"
    "sqrt_sfp32.f90"
    "sqrt_sfp64.f90"
    "tanh_r1fp32.f90"
    "tanh_r1fp64.f90"
    "tanh_r2fp32.f90"
    "tanh_r2fp64.f90"
    "tanh_r3fp32.f90"
    "tanh_r3fp64.f90"
    "tanh_r4fp32.f90"
    "tanh_r4fp64.f90"
    "tanh_sfp32.f90"
    "tanh_sfp64.f90"
    "tan_r1fp32.f90"
    "tan_r1fp64.f90"
    "tan_r2fp32.f90"
    "tan_r2fp64.f90"
    "tan_r3fp32.f90"
    "tan_r3fp64.f90"
    "tan_r4fp32.f90"
    "tan_r4fp64.f90"
    "tan_sfp32.f90"
    "tan_sfp64.f90")
    
# ! //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////// !
# !
# ! Maintainers : support@fluidnumerics.com
# ! Official Repository : https://github.com/FluidNumerics/feq-parse/
# !
# ! Copyright © 2024 Fluid Numerics LLC
# !
# ! Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:
# !
# ! 1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
# !
# ! 2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in 
# !    the documentation and/or other materials provided with the distribution.
# !
# ! 3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived from 
# !    this software without specific prior written permission.
# !
# ! THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS “AS IS” AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# ! LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
# ! HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT 
# ! LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY 
# ! THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF 
# ! THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
# !
# ! //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////// !
CMAKE_MINIMUM_REQUIRED(VERSION 3.0.2)

set(CMAKE_VERBOSE_MAKEFILE ON)
set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/include)
set(make_env FEQPARSE_HOME=${CMAKE_BINARY_DIR} FEQPARSE_COMPILER=${FEQPARSE_COMPILER})
enable_testing()

function (add_fortran_tests)

    foreach (TFILE ${ARGN})
        string (REGEX REPLACE ".f90$" "" TEST_NAME ${TFILE})
        add_executable (${TEST_NAME} ${CMAKE_CURRENT_SOURCE_DIR}/${TFILE})
        target_link_libraries (${TEST_NAME} feqparse-static)
        target_include_directories(${TEST_NAME} PRIVATE ${CMAKE_Fortran_MODULE_DIRECTORY}/)
        add_test(NAME ${TEST_NAME} COMMAND ${TEST_NAME})
        install(TARGETS ${TEST_NAME} DESTINATION example)
    endforeach ()

endfunction ()

add_fortran_tests (
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
    "cosh_r1fp32.f90"
    "cosh_r1fp64.f90"
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
    "sinh_r1fp32.f90"
    "sinh_r1fp64.f90"
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
    "tan_sfp64.f90"
    "print_tokens.f90"
    "custom_r1fp64.f90"
    "parsing_difficult_r1fp64.f90"
    "parsing_vanderpol_sfp64.f90")
    
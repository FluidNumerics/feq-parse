program test

    !! a main program to include the test case, so it
    !! can be compiled with FPM.

    implicit none

    write(*,*) gaussian3d_sfp32()
    write(*,*) gaussian3d_sfp64()

    contains

    include "abs_r1fp32.f90"
    include "abs_r1fp64.f90"
    include "abs_sfp32.f90"
    include "abs_sfp64.f90"
    include "acos_r1fp32.f90"
    include "acos_r1fp64.f90"
    include "acos_sfp32.f90"
    include "acos_sfp64.f90"
    include "asin_r1fp32.f90"
    include "asin_r1fp64.f90"
    include "asin_sfp32.f90"
    include "asin_sfp64.f90"
    include "atan_r1fp32.f90"
    include "atan_r1fp64.f90"
    include "atan_sfp32.f90"
    include "atan_sfp64.f90"
    include "cos_r1fp32.f90"
    include "cos_r1fp64.f90"
    include "cos_sfp32.f90"
    include "cos_sfp64.f90"
    include "gaussian3d_r1fp32.f90"
    include "gaussian3d_r1fp64.f90"
    include "gaussian3d_sfp32.f90"
    include "gaussian3d_sfp64.f90"
    include "linear_r1fp32.f90"
    include "linear_r1fp64.f90"
    include "log10_r1fp32.f90"
    include "log10_r1fp64.f90"
    include "log10_sfp32.f90"
    include "log10_sfp64.f90"
    include "log_r1fp32.f90"
    include "log_r1fp64.f90"
    include "log_sfp32.f90"
    include "log_sfp64.f90"
    include "random_r1fp32.f90"
    include "random_r1fp64.f90"
    include "random_sfp32.f90"
    include "random_sfp64.f90"
    include "sech_r1fp32.f90"
    include "sech_r1fp64.f90"
    include "sech_sfp32.f90"
    include "sech_sfp64.f90"
    include "sin_r1fp32.f90"
    include "sin_r1fp64.f90"
    include "sin_sfp32.f90"
    include "sin_sfp64.f90"
    include "sqrt_r1fp32.f90"
    include "sqrt_r1fp64.f90"
    include "sqrt_sfp32.f90"
    include "sqrt_sfp64.f90"
    include "tanh_r1fp32.f90"
    include "tanh_r1fp64.f90"
    include "tanh_sfp32.f90"
    include "tanh_sfp64.f90"
    include "tan_r1fp32.f90"
    include "tan_r1fp64.f90"
    include "tan_sfp32.f90"
    include "tan_sfp64.f90"
    




end program test
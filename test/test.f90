program test

    !! a main program to include the test case, so it
    !! can be compiled with FPM.

    implicit none

    write(*,*) gaussian3d_sfp32()
    write(*,*) gaussian3d_sfp64()

    contains

    
    include "gaussian3d_sfp32.f90"
    include "cos_sfp32.f90"
    include "sin_sfp32.f90"
    include "tan_sfp32.f90"
    include "tanh_sfp32.f90"

    include "gaussian3d_sfp64.f90"
    include "cos_sfp64.f90"
    include "sin_sfp64.f90"
    include "tan_sfp64.f90"
    include "tanh_sfp64.f90"

    include "linear_r1fp32.f90"
    include "linear_r1fp64.f90"

    include "gaussian3d_r1fp32.f90"
    include "cos_r1fp32.f90"
    include "sin_r1fp32.f90"
    include "tan_r1fp32.f90"
    include "tanh_r1fp32.f90"

    include "gaussian3d_r1fp64.f90"
    include "cos_r1fp64.f90"
    include "sin_r1fp64.f90"
    include "tan_r1fp64.f90"
    include "tanh_r1fp64.f90"




end program test
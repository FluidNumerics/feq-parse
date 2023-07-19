program test

    !! a main program to include the test case, so it
    !! can be compiled with FPM.

    implicit none

    write(*,*) gaussian3d()
    write(*,*) tanh_with_t()

    contains

    include "gaussian3d.f90"
    include "tanh_with_t.f90"

end program test
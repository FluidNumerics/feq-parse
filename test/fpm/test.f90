program test

    !! a main program to include the test case, so it
    !! can be compiled with FPM.
    use FEQParse
    implicit none

    write(*,*) "abs_r1fp32()", abs_r1fp32()
    write(*,*) "abs_r1fp64()", abs_r1fp64()
    write(*,*) "abs_r2fp32()", abs_r2fp32()
    write(*,*) "abs_r2fp64()", abs_r2fp64()
    write(*,*) "abs_r3fp32()", abs_r3fp32()
    write(*,*) "abs_r3fp64()", abs_r3fp64()
    write(*,*) "abs_r4fp32()", abs_r4fp32()
    write(*,*) "abs_r4fp64()", abs_r4fp64()
    write(*,*) "abs_sfp32()", abs_sfp32()
    write(*,*) "abs_sfp64()", abs_sfp64()
    write(*,*) "acos_r1fp32()", acos_r1fp32()
    write(*,*) "acos_r1fp64()", acos_r1fp64()
    write(*,*) "acos_r2fp32()", acos_r2fp32()
    write(*,*) "acos_r2fp64()", acos_r2fp64()
    write(*,*) "acos_r3fp32()", acos_r3fp32()
    write(*,*) "acos_r3fp64()", acos_r3fp64()
    write(*,*) "acos_r4fp32()", acos_r4fp32()
    write(*,*) "acos_r4fp64()", acos_r4fp64()
    write(*,*) "acos_sfp32()", acos_sfp32()
    write(*,*) "acos_sfp64()", acos_sfp64()
    write(*,*) "asin_r1fp32()", asin_r1fp32()
    write(*,*) "asin_r1fp64()", asin_r1fp64()
    write(*,*) "asin_r2fp32()", asin_r2fp32()
    write(*,*) "asin_r2fp64()", asin_r2fp64()
    write(*,*) "asin_r3fp32()", asin_r3fp32()
    write(*,*) "asin_r3fp64()", asin_r3fp64()
    write(*,*) "asin_r4fp32()", asin_r4fp32()
    write(*,*) "asin_r4fp64()", asin_r4fp64()
    write(*,*) "asin_sfp32()", asin_sfp32()
    write(*,*) "asin_sfp64()", asin_sfp64()
    write(*,*) "atan_r1fp32()", atan_r1fp32()
    write(*,*) "atan_r1fp64()", atan_r1fp64()
    write(*,*) "atan_r2fp32()", atan_r2fp32()
    write(*,*) "atan_r2fp64()", atan_r2fp64()
    write(*,*) "atan_r3fp32()", atan_r3fp32()
    write(*,*) "atan_r3fp64()", atan_r3fp64()
    write(*,*) "atan_r4fp32()", atan_r4fp32()
    write(*,*) "atan_r4fp64()", atan_r4fp64()
    write(*,*) "atan_sfp32()", atan_sfp32()
    write(*,*) "atan_sfp64()", atan_sfp64()
    write(*,*) "cos_r1fp32()", cos_r1fp32()
    write(*,*) "cos_r1fp64()", cos_r1fp64()
    write(*,*) "cos_r2fp32()", cos_r2fp32()
    write(*,*) "cos_r2fp64()", cos_r2fp64()
    write(*,*) "cos_r3fp32()", cos_r3fp32()
    write(*,*) "cos_r3fp64()", cos_r3fp64()
    write(*,*) "cos_r4fp32()", cos_r4fp32()
    write(*,*) "cos_r4fp64()", cos_r4fp64()
    write(*,*) "cos_sfp32()", cos_sfp32()
    write(*,*) "cos_sfp64()", cos_sfp64()
    write(*,*) "division_r1fp32()", division_r1fp32()
    write(*,*) "division_r1fp64()", division_r1fp64()
    write(*,*) "division_r2fp32()", division_r2fp32()
    write(*,*) "division_r2fp64()", division_r2fp64()
    write(*,*) "division_r3fp32()", division_r3fp32()
    write(*,*) "division_r3fp64()", division_r3fp64()
    write(*,*) "division_r4fp32()", division_r4fp32()
    write(*,*) "division_r4fp64()", division_r4fp64()
    write(*,*) "division_sfp32()", division_sfp32()
    write(*,*) "division_sfp64()", division_sfp64()
    write(*,*) "gaussian3d_r1fp32()", gaussian3d_r1fp32()
    write(*,*) "gaussian3d_r1fp64()", gaussian3d_r1fp64()
    write(*,*) "gaussian3d_r2fp32()", gaussian3d_r2fp32()
    write(*,*) "gaussian3d_r2fp64()", gaussian3d_r2fp64()
    write(*,*) "gaussian3d_r3fp32()", gaussian3d_r3fp32()
    write(*,*) "gaussian3d_r3fp64()", gaussian3d_r3fp64()
    write(*,*) "gaussian3d_r4fp32()", gaussian3d_r4fp32()
    write(*,*) "gaussian3d_r4fp64()", gaussian3d_r4fp64()
    write(*,*) "gaussian3d_sfp32()", gaussian3d_sfp32()
    write(*,*) "gaussian3d_sfp64()", gaussian3d_sfp64()
    write(*,*) "linear_r1fp32()", linear_r1fp32()
    write(*,*) "linear_r1fp64()", linear_r1fp64()
    write(*,*) "linear_r2fp32()", linear_r2fp32()
    write(*,*) "linear_r2fp64()", linear_r2fp64()
    write(*,*) "linear_r3fp32()", linear_r3fp32()
    write(*,*) "linear_r3fp64()", linear_r3fp64()
    write(*,*) "linear_r4fp32()", linear_r4fp32()
    write(*,*) "linear_r4fp64()", linear_r4fp64()
    write(*,*) "log10_r1fp32()", log10_r1fp32()
    write(*,*) "log10_r1fp64()", log10_r1fp64()
    write(*,*) "log10_r2fp32()", log10_r2fp32()
    write(*,*) "log10_r2fp64()", log10_r2fp64()
    write(*,*) "log10_r3fp32()", log10_r3fp32()
    write(*,*) "log10_r3fp64()", log10_r3fp64()
    write(*,*) "log10_r4fp32()", log10_r4fp32()
    write(*,*) "log10_r4fp64()", log10_r4fp64()
    write(*,*) "log10_sfp32()", log10_sfp32()
    write(*,*) "log10_sfp64()", log10_sfp64()
    write(*,*) "log_r1fp32()", log_r1fp32()
    write(*,*) "log_r1fp64()", log_r1fp64()
    write(*,*) "log_r2fp32()", log_r2fp32()
    write(*,*) "log_r2fp64()", log_r2fp64()
    write(*,*) "log_r3fp32()", log_r3fp32()
    write(*,*) "log_r3fp64()", log_r3fp64()
    write(*,*) "log_r4fp32()", log_r4fp32()
    write(*,*) "log_r4fp64()", log_r4fp64()
    write(*,*) "log_sfp32()", log_sfp32()
    write(*,*) "log_sfp64()", log_sfp64()
    write(*,*) "monadic_r1fp32()", monadic_r1fp32()
    write(*,*) "monadic_r1fp64()", monadic_r1fp64()
    write(*,*) "monadic_r2fp32()", monadic_r2fp32()
    write(*,*) "monadic_r2fp64()", monadic_r2fp64()
    write(*,*) "monadic_r3fp32()", monadic_r3fp32()
    write(*,*) "monadic_r3fp64()", monadic_r3fp64()
    write(*,*) "monadic_r4fp32()", monadic_r4fp32()
    write(*,*) "monadic_r4fp64()", monadic_r4fp64()
    write(*,*) "monadic_sfp32()", monadic_sfp32()
    write(*,*) "monadic_sfp64()", monadic_sfp64()
    write(*,*) "random_r1fp32()", random_r1fp32()
    write(*,*) "random_r1fp64()", random_r1fp64()
    write(*,*) "random_r2fp32()", random_r2fp32()
    write(*,*) "random_r2fp64()", random_r2fp64()
    write(*,*) "random_r3fp32()", random_r3fp32()
    write(*,*) "random_r3fp64()", random_r3fp64()
    write(*,*) "random_r4fp32()", random_r4fp32()
    write(*,*) "random_r4fp64()", random_r4fp64()
    write(*,*) "random_sfp32()", random_sfp32()
    write(*,*) "random_sfp64()", random_sfp64()
    write(*,*) "sech_r1fp32()", sech_r1fp32()
    write(*,*) "sech_r1fp64()", sech_r1fp64()
    write(*,*) "sech_r2fp32()", sech_r2fp32()
    write(*,*) "sech_r2fp64()", sech_r2fp64()
    write(*,*) "sech_r3fp32()", sech_r3fp32()
    write(*,*) "sech_r3fp64()", sech_r3fp64()
    write(*,*) "sech_r4fp32()", sech_r4fp32()
    write(*,*) "sech_r4fp64()", sech_r4fp64()
    write(*,*) "sech_sfp32()", sech_sfp32()
    write(*,*) "sech_sfp64()", sech_sfp64()
    write(*,*) "sin_r1fp32()", sin_r1fp32()
    write(*,*) "sin_r1fp64()", sin_r1fp64()
    write(*,*) "sin_r2fp32()", sin_r2fp32()
    write(*,*) "sin_r2fp64()", sin_r2fp64()
    write(*,*) "sin_r3fp32()", sin_r3fp32()
    write(*,*) "sin_r3fp64()", sin_r3fp64()
    write(*,*) "sin_r4fp32()", sin_r4fp32()
    write(*,*) "sin_r4fp64()", sin_r4fp64()
    write(*,*) "sin_sfp32()", sin_sfp32()
    write(*,*) "sin_sfp64()", sin_sfp64()
    write(*,*) "sqrt_r1fp32()", sqrt_r1fp32()
    write(*,*) "sqrt_r1fp64()", sqrt_r1fp64()
    write(*,*) "sqrt_r2fp32()", sqrt_r2fp32()
    write(*,*) "sqrt_r2fp64()", sqrt_r2fp64()
    write(*,*) "sqrt_r3fp32()", sqrt_r3fp32()
    write(*,*) "sqrt_r3fp64()", sqrt_r3fp64()
    write(*,*) "sqrt_r4fp32()", sqrt_r4fp32()
    write(*,*) "sqrt_r4fp64()", sqrt_r4fp64()
    write(*,*) "sqrt_sfp32()", sqrt_sfp32()
    write(*,*) "sqrt_sfp64()", sqrt_sfp64()
    write(*,*) "tanh_r1fp32()", tanh_r1fp32()
    write(*,*) "tanh_r1fp64()", tanh_r1fp64()
    write(*,*) "tanh_r2fp32()", tanh_r2fp32()
    write(*,*) "tanh_r2fp64()", tanh_r2fp64()
    write(*,*) "tanh_r3fp32()", tanh_r3fp32()
    write(*,*) "tanh_r3fp64()", tanh_r3fp64()
    write(*,*) "tanh_r4fp32()", tanh_r4fp32()
    write(*,*) "tanh_r4fp64()", tanh_r4fp64()
    write(*,*) "tanh_sfp32()", tanh_sfp32()
    write(*,*) "tanh_sfp64()", tanh_sfp64()
    write(*,*) "tan_r1fp32()", tan_r1fp32()
    write(*,*) "tan_r1fp64()", tan_r1fp64()
    write(*,*) "tan_r2fp32()", tan_r2fp32()
    write(*,*) "tan_r2fp64()", tan_r2fp64()
    write(*,*) "tan_r3fp32()", tan_r3fp32()
    write(*,*) "tan_r3fp64()", tan_r3fp64()
    write(*,*) "tan_r4fp32()", tan_r4fp32()
    write(*,*) "tan_r4fp64()", tan_r4fp64()
    write(*,*) "tan_sfp32()", tan_sfp32()
    write(*,*) "tan_sfp64()", tan_sfp64()

    contains

    include "../abs_r1fp32.f90"
    include "../abs_r1fp64.f90"
    include "../abs_r2fp32.f90"
    include "../abs_r2fp64.f90"
    include "../abs_r3fp32.f90"
    include "../abs_r3fp64.f90"
    include "../abs_r4fp32.f90"
    include "../abs_r4fp64.f90"
    include "../abs_sfp32.f90"
    include "../abs_sfp64.f90"
    include "../acos_r1fp32.f90"
    include "../acos_r1fp64.f90"
    include "../acos_r2fp32.f90"
    include "../acos_r2fp64.f90"
    include "../acos_r3fp32.f90"
    include "../acos_r3fp64.f90"
    include "../acos_r4fp32.f90"
    include "../acos_r4fp64.f90"
    include "../acos_sfp32.f90"
    include "../acos_sfp64.f90"
    include "../asin_r1fp32.f90"
    include "../asin_r1fp64.f90"
    include "../asin_r2fp32.f90"
    include "../asin_r2fp64.f90"
    include "../asin_r3fp32.f90"
    include "../asin_r3fp64.f90"
    include "../asin_r4fp32.f90"
    include "../asin_r4fp64.f90"
    include "../asin_sfp32.f90"
    include "../asin_sfp64.f90"
    include "../atan_r1fp32.f90"
    include "../atan_r1fp64.f90"
    include "../atan_r2fp32.f90"
    include "../atan_r2fp64.f90"
    include "../atan_r3fp32.f90"
    include "../atan_r3fp64.f90"
    include "../atan_r4fp32.f90"
    include "../atan_r4fp64.f90"
    include "../atan_sfp32.f90"
    include "../atan_sfp64.f90"
    include "../cos_r1fp32.f90"
    include "../cos_r1fp64.f90"
    include "../cos_r2fp32.f90"
    include "../cos_r2fp64.f90"
    include "../cos_r3fp32.f90"
    include "../cos_r3fp64.f90"
    include "../cos_r4fp32.f90"
    include "../cos_r4fp64.f90"
    include "../cos_sfp32.f90"
    include "../cos_sfp64.f90"
    include "../division_r1fp32.f90"
    include "../division_r1fp64.f90"
    include "../division_r2fp32.f90"
    include "../division_r2fp64.f90"
    include "../division_r3fp32.f90"
    include "../division_r3fp64.f90"
    include "../division_r4fp32.f90"
    include "../division_r4fp64.f90"
    include "../division_sfp32.f90"
    include "../division_sfp64.f90"
    include "../gaussian3d_r1fp32.f90"
    include "../gaussian3d_r1fp64.f90"
    include "../gaussian3d_r2fp32.f90"
    include "../gaussian3d_r2fp64.f90"
    include "../gaussian3d_r3fp32.f90"
    include "../gaussian3d_r3fp64.f90"
    include "../gaussian3d_r4fp32.f90"
    include "../gaussian3d_r4fp64.f90"
    include "../gaussian3d_sfp32.f90"
    include "../gaussian3d_sfp64.f90"
    include "../linear_r1fp32.f90"
    include "../linear_r1fp64.f90"
    include "../linear_r2fp32.f90"
    include "../linear_r2fp64.f90"
    include "../linear_r3fp32.f90"
    include "../linear_r3fp64.f90"
    include "../linear_r4fp32.f90"
    include "../linear_r4fp64.f90"
    include "../log10_r1fp32.f90"
    include "../log10_r1fp64.f90"
    include "../log10_r2fp32.f90"
    include "../log10_r2fp64.f90"
    include "../log10_r3fp32.f90"
    include "../log10_r3fp64.f90"
    include "../log10_r4fp32.f90"
    include "../log10_r4fp64.f90"
    include "../log10_sfp32.f90"
    include "../log10_sfp64.f90"
    include "../log_r1fp32.f90"
    include "../log_r1fp64.f90"
    include "../log_r2fp32.f90"
    include "../log_r2fp64.f90"
    include "../log_r3fp32.f90"
    include "../log_r3fp64.f90"
    include "../log_r4fp32.f90"
    include "../log_r4fp64.f90"
    include "../log_sfp32.f90"
    include "../log_sfp64.f90"
    include "../monadic_r1fp32.f90"
    include "../monadic_r1fp64.f90"
    include "../monadic_r2fp32.f90"
    include "../monadic_r2fp64.f90"
    include "../monadic_r3fp32.f90"
    include "../monadic_r3fp64.f90"
    include "../monadic_r4fp32.f90"
    include "../monadic_r4fp64.f90"
    include "../monadic_sfp32.f90"
    include "../monadic_sfp64.f90"
    include "../random_r1fp32.f90"
    include "../random_r1fp64.f90"
    include "../random_r2fp32.f90"
    include "../random_r2fp64.f90"
    include "../random_r3fp32.f90"
    include "../random_r3fp64.f90"
    include "../random_r4fp32.f90"
    include "../random_r4fp64.f90"
    include "../random_sfp32.f90"
    include "../random_sfp64.f90"
    include "../sech_r1fp32.f90"
    include "../sech_r1fp64.f90"
    include "../sech_r2fp32.f90"
    include "../sech_r2fp64.f90"
    include "../sech_r3fp32.f90"
    include "../sech_r3fp64.f90"
    include "../sech_r4fp32.f90"
    include "../sech_r4fp64.f90"
    include "../sech_sfp32.f90"
    include "../sech_sfp64.f90"
    include "../sin_r1fp32.f90"
    include "../sin_r1fp64.f90"
    include "../sin_r2fp32.f90"
    include "../sin_r2fp64.f90"
    include "../sin_r3fp32.f90"
    include "../sin_r3fp64.f90"
    include "../sin_r4fp32.f90"
    include "../sin_r4fp64.f90"
    include "../sin_sfp32.f90"
    include "../sin_sfp64.f90"
    include "../sqrt_r1fp32.f90"
    include "../sqrt_r1fp64.f90"
    include "../sqrt_r2fp32.f90"
    include "../sqrt_r2fp64.f90"
    include "../sqrt_r3fp32.f90"
    include "../sqrt_r3fp64.f90"
    include "../sqrt_r4fp32.f90"
    include "../sqrt_r4fp64.f90"
    include "../sqrt_sfp32.f90"
    include "../sqrt_sfp64.f90"
    include "../tanh_r1fp32.f90"
    include "../tanh_r1fp64.f90"
    include "../tanh_r2fp32.f90"
    include "../tanh_r2fp64.f90"
    include "../tanh_r3fp32.f90"
    include "../tanh_r3fp64.f90"
    include "../tanh_r4fp32.f90"
    include "../tanh_r4fp64.f90"
    include "../tanh_sfp32.f90"
    include "../tanh_sfp64.f90"
    include "../tan_r1fp32.f90"
    include "../tan_r1fp64.f90"
    include "../tan_r2fp32.f90"
    include "../tan_r2fp64.f90"
    include "../tan_r3fp32.f90"
    include "../tan_r3fp64.f90"
    include "../tan_r4fp32.f90"
    include "../tan_r4fp64.f90"
    include "../tan_sfp32.f90"
    include "../tan_sfp64.f90"

 end program test

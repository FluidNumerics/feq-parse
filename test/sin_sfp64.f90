program test

  implicit none
  integer :: exit_code

  exit_code = sin_sfp64()
  stop exit_code

contains

  integer function sin_sfp64() result(r)
    use FEQParse
    use iso_fortran_env
    implicit none
    integer,parameter :: N = 10
    real(real64),parameter :: pi = 4.0_real64*atan(1.0_real64)
    type(EquationParser) :: f
    character(LEN=1),dimension(1:3) :: independentVars
    character(LEN=2048) :: eqChar
    real(real64) :: x(1:3)
    real(real64) :: feval
    real(real64) :: fexact
    integer :: i

    ! Specify the independent variables
    independentVars = (/'x','y','z'/)

    ! Specify an equation string that we want to evaluate
    eqChar = 'f = sin( 2.0*pi*x )'

    ! Create the EquationParser object
    f = EquationParser(eqChar,independentVars)

    x = 0.0_real64
    fexact = sin(2.0_real64*pi*x(1))

    ! Evaluate the equation
    feval = f%evaluate(x)
    if((abs(feval-fexact)) <= epsilon(1.0_real64)) then
      r = 0
    else
      r = 1
    endif

  endfunction sin_sfp64
endprogram test

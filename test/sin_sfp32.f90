
integer function sin_sfp32() result(r)
  use FEQParse
  use iso_fortran_env
  implicit none
  integer,parameter :: N = 1000
  real(real32),parameter :: pi = 4.0_real32*atan(1.0_real32)
  type(EquationParser) :: f
  character(LEN=1),dimension(1:3) :: independentVars
  character(LEN=2048) :: eqChar
  real(real32) :: x(1:3)
  real(real32) :: feval
  real(real32) :: fexact
  integer :: i

  ! Specify the independent variables
  independentVars = (/'x','y','z'/)

  ! Specify an equation string that we want to evaluate
  eqChar = 'f = sin( 2.0*pi*x )'

  ! Create the EquationParser object
  f = EquationParser(eqChar,independentVars)

  x = 0.0_real32
  fexact = sin(2.0_real32*pi*x(1))

  ! Evaluate the equation
  feval = f % evaluate(x)
  if ((abs(feval - fexact)) <= epsilon(1.0_real32)) then
    r = 0
  else
    r = 1
  end if

  ! Clean up memory
  call f % Destruct()

end function sin_sfp32

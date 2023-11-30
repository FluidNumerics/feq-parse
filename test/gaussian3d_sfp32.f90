
integer function gaussian3d_sfp32() result(r)
  use FEQParse
  use iso_fortran_env
  implicit none

  type(EquationParser) :: f
  character(LEN=1),dimension(1:3) :: independentVars
  character(LEN=2048) :: eqChar
  real(real32) :: x(1:3)

  ! Specify the independent variables
  independentVars = (/'x','y','z'/)

  ! Specify an equation string that we want to evaluate
  eqChar = 'f = \exp( -(x^2 + y^2 + z^2) )'

  ! Create the EquationParser object
  f = EquationParser(eqChar,independentVars)

  ! Evaluate the equation
  x = (/0.0,0.0,0.0/)
  if (abs(f % evaluate(x) - 1.0) <= epsilon(1.0)) then
    r = 0
  else
    r = 1
  end if

  ! Clean up memory
  call f % Destruct()

end function gaussian3d_sfp32

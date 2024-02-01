program gaussian_scalar_multivar
  use iso_fortran_env
  use FEQParse

  implicit none
  type(EquationParser) :: f
  character(LEN=1),dimension(2) :: independentVars
  character(LEN=30) :: eqChar
  real :: x(2)

  ! Specify the independent variables
  independentVars = (/'x', 'a'/)

  ! Specify an equation string that we want to evaluate
  eqChar = 'f = exp( -(x^2) ) - a'
  ! eqChar = 'f = exp( -(x^2) - a )'
  ! eqChar = 'f = (x - a)^2 )'

  ! Create the EquationParser object
  f = EquationParser(eqChar,independentVars)

  ! Evaluate the equation
  x(1) = 1.0
  x(2) = 1.0
  print*, f % evaluate(x)
  print*, exp(-1.0) -1.0

  end program gaussian_scalar_multivar
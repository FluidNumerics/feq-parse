program scalar_with_scalar_eval
  use iso_fortran_env
  use FEQParse

  implicit none
  type(EquationParser) :: f
  character(LEN=1),dimension(1) :: independentVars
  character(LEN=30) :: eqChar
  real :: x(1)

  ! Specify the independent variables
  independentVars = (/'x'/)

  ! Specify an equation string that we want to evaluate
  eqChar = 'f = exp( -(x^2) )'

  ! Create the EquationParser object
  f = EquationParser(eqChar,independentVars)

  ! Evaluate the equation
  x(1) = 0.0
  print*,f%evaluate(x)

endprogram scalar_with_scalar_eval

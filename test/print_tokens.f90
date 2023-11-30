
integer function print_tokens() result(r)
  use FEQParse
  use iso_fortran_env
  implicit none
  type(EquationParser) :: f
  character(LEN=1),dimension(1:3) :: independentVars
  character(LEN=2048) :: eqChar

  ! Specify the independent variables
  independentVars = (/'x','y','z'/)

  ! Specify an equation string that we want to evaluate
  eqChar = 'f = -(cos( 2.0*pi*x ) + 5.0)/(sin(2.0*pi*y) + 10.0)*tanh(z)'

  ! Create the EquationParser object
  f = EquationParser(eqChar,independentVars)
  call f % Print_InfixTokens()
  call f % Print_PostfixTokens()

  ! Clean up memory
  call f % Destruct()
  r=0

end function print_tokens

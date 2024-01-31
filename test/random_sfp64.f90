
integer function random_sfp64() result(r)
  use FEQParse
  use iso_fortran_env
  implicit none
  integer,parameter :: N = 1000
  type(EquationParser) :: f
  character(LEN=1),dimension(1:3) :: independentVars
  character(LEN=2048) :: eqChar
  real(real64) :: x(1:3)
  real(real64) :: feval
  integer :: i

  ! Specify the independent variables
  independentVars = (/'x','y','z'/)

  ! Specify an equation string that we want to evaluate
  eqChar = 'f = random( x )'

  ! Create the EquationParser object
  f = EquationParser(eqChar,independentVars)

  x = 0.0_real64

  ! Evaluate the equation
  feval = f % evaluate(x)
  if ((abs(feval)) <= 1.0_real64) then
    r = 0
  else
    r = 1
  end if

  ! Clean up memory
  call f % Destruct()

end function random_sfp64

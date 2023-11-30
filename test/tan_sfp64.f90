
integer function tan_sfp64() result(r)
  use FEQParse
  use iso_fortran_env
  implicit none
  integer,parameter :: N = 1000
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
  eqChar = 'f = \tan( 0.5*pi*x )'

  ! Create the EquationParser object
  f = EquationParser(eqChar,independentVars)

  x = 0.0_real64
  fexact = tan(0.5_real64*pi*x(1))

  ! Evaluate the equation
  feval = f % evaluate(x)
  if ((abs(feval - fexact)) <= epsilon(1.0_real64)) then
    r = 0
  else
    r = 1
  end if

  ! Clean up memory
  call f % Destruct()

end function tan_sfp64

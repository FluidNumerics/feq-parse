program test

  implicit none
  integer :: exit_code
  
  exit_code = division_r1fp64()
  stop exit_code

contains

integer function division_r1fp64() result(r)
  use FEQParse
  use iso_fortran_env
  implicit none
  integer,parameter :: N = 10
  type(EquationParser) :: f
  character(LEN=1),dimension(1:3) :: independentVars
  character(LEN=2048) :: eqChar
  real(real64) :: x(1:N,1:3)
  real(real64) :: feval(1:N)
  real(real64) :: fexact(1:N)
  integer :: i

  ! Specify the independent variables
  independentVars = (/'x','y','z'/)

  ! Specify an equation string that we want to evaluate
  eqChar = 'f = -x/10.0'

  ! Create the EquationParser object
  f = EquationParser(eqChar,independentVars)

  x = 0.0_real64
  do i = 1,N
    x(i,1) = -1.0_real64 + (2.0_real64)/real(N,real64)*real(i - 1,real64)
    fexact(i) = -x(i,1)/10.0_real64
  end do

  ! Evaluate the equation
  feval = f % evaluate(x)
  if (maxval(abs(feval - fexact)) <= epsilon(1.0_real64)) then
    r = 0
  else
    r = 1
  end if

end function division_r1fp64
end program test

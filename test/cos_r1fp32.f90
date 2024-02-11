program test

  implicit none
  integer :: exit_code
  
  exit_code = cos_r1fp32()
  stop exit_code

contains

integer function cos_r1fp32() result(r)
  use FEQParse
  use iso_fortran_env
  implicit none
  integer,parameter :: N = 10
  real(real32),parameter :: pi = 4.0_real32*atan(1.0_real32)
  type(EquationParser) :: f
  character(LEN=1),dimension(1:3) :: independentVars
  character(LEN=2048) :: eqChar
  real(real32) :: x(1:N,1:3)
  real(real32) :: feval(1:N)
  real(real32) :: fexact(1:N)
  integer :: i

  ! Specify the independent variables
  independentVars = (/'x','y','z'/)

  ! Specify an equation string that we want to evaluate
  eqChar = 'f = cos( 2.0*pi*x )'

  ! Create the EquationParser object
  f = EquationParser(eqChar,independentVars)

  x = 0.0_real32
  do i = 1,N
    x(i,1) = -1.0_real32 + (2.0_real32)/real(N,real32)*real(i - 1,real32)
    fexact(i) = cos(2.0_real32*pi*x(i,1))
  end do

  ! Evaluate the equation
  feval = f % evaluate(x)
  if (maxval(abs(feval - fexact)) <= epsilon(1.0_real32)) then
    r = 0
  else
    r = 1
  end if

end function cos_r1fp32
end program test

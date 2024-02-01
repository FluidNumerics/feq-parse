
integer function gaussian3d_r1fp32() result(r)
  use FEQParse
  use iso_fortran_env
  implicit none
  integer,parameter :: N = 10
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
  eqChar = 'f = exp( -(x^2 + y^2 + z^2) )'

  ! Create the EquationParser object
  f = EquationParser(eqChar,independentVars)

  x = 0.0_real32
  do i = 1,N
    x(i,1) = -1.0_real32 + (2.0_real32)/real(N,real32)*real(i - 1,real32)
    x(i,2) = -1.0_real32 + (2.0_real32)/real(N,real32)*real(i - 1,real32)
    x(i,3) = -1.0_real32 + (2.0_real32)/real(N,real32)*real(i - 1,real32)
    fexact(i) = exp(-(x(i,1)**2 + x(i,2)**2 + x(i,3)**2))
  end do

  ! Evaluate the equation
  feval = f % evaluate(x)
  if (maxval(abs(feval - fexact)) <= epsilon(1.0_real32)) then
    r = 0
  else
    r = 1
  end if

end function gaussian3d_r1fp32

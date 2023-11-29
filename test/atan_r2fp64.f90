
integer function atan_r2fp64() result(r)
  use FEQParse
  use iso_fortran_env
  implicit none
  integer,parameter :: N = 1000
  type(EquationParser) :: f
  character(LEN=1),dimension(1:3) :: independentVars
  character(LEN=1024) :: eqChar
  real(real64) :: x(1:N,1:N,1:3)
  real(real64) :: feval(1:N,1:N)
  real(real64) :: fexact(1:N,1:N)
  integer :: i,j

  ! Specify the independent variables
  independentVars = (/'x','y','z'/)

  ! Specify an equation string that we want to evaluate
  eqChar = 'f = \atan( x )*\atan( y )'

  ! Create the EquationParser object
  f = EquationParser(eqChar,independentVars)

  x = 0.0_real64
  do j = 1,N
    do i = 1,N
      x(i,j,1) = -1.0_real32 + (2.0_real32)/real(N,real32)*real(i - 1,real32)
      x(i,j,2) = -1.0_real32 + (2.0_real32)/real(N,real32)*real(j - 1,real32)
      fexact(i,j) = atan(x(i,j,1))*atan(x(i,j,2))
    end do
  end do

  ! Evaluate the equation
  feval = f % evaluate(x)
  if (maxval(abs(feval - fexact)) <= epsilon(1.0)) then
    r = 0
  else
    r = 1
  end if

  ! Clean up memory
  call f % Destruct()

end function atan_r2fp64

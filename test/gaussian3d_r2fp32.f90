program test

  implicit none
  integer :: exit_code
  
  exit_code = gaussian3d_r2fp32()
  stop exit_code

contains

integer function gaussian3d_r2fp32() result(r)
  use FEQParse
  use iso_fortran_env
  implicit none
  integer,parameter :: N = 10
  type(EquationParser) :: f
  character(LEN=1),dimension(1:3) :: independentVars
  character(LEN=1024) :: eqChar
  real(real32),allocatable :: x(:,:,:)
  real(real32),allocatable :: feval(:,:)
  real(real32),allocatable :: fexact(:,:)
  integer :: i,j

  allocate (x(1:N,1:N,1:3), &
            feval(1:N,1:N), &
            fexact(1:N,1:N))

  ! Specify the independent variables
  independentVars = (/'x','y','z'/)

  ! Specify an equation string that we want to evaluate
  eqChar = 'f = exp( -(x^2 + y^2) )'

  ! Create the EquationParser object
  f = EquationParser(eqChar,independentVars)

  x = 0.0_real32
  do j = 1,N
    do i = 1,N
      x(i,j,1) = -1.0_real32 + (2.0_real32)/real(N,real32)*real(i - 1,real32)
      x(i,j,2) = -1.0_real32 + (2.0_real32)/real(N,real32)*real(j - 1,real32)
      fexact(i,j) = exp(-(x(i,j,1)**2 + x(i,j,2)**2))
    end do
  end do

  ! Evaluate the equation
  feval = f % evaluate(x)
  if (maxval(abs(feval - fexact)) <= epsilon(1.0_real32)) then
    r = 0
  else
    r = 1
  end if
  
  deallocate (x,feval,fexact)

end function gaussian3d_r2fp32
end program test

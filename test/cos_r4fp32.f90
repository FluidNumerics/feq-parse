program test

  implicit none
  integer :: exit_code
  
  exit_code = cos_r4fp32()
  stop exit_code

contains

integer function cos_r4fp32() result(r)
  use FEQParse
  use iso_fortran_env
  implicit none
  real(real32),parameter :: pi = 4.0_real32*atan(1.0_real32)
  integer,parameter :: N = 2
  integer,parameter :: M = 5
  type(EquationParser) :: f
  character(LEN=1),dimension(1:3) :: independentVars
  character(LEN=1024) :: eqChar
  real(real32),allocatable :: x(:,:,:,:,:)
  real(real32),allocatable :: feval(:,:,:,:)
  real(real32),allocatable :: fexact(:,:,:,:)
  integer :: i,j,k,l

  allocate (x(1:N,1:N,1:N,1:M,1:3), &
            feval(1:N,1:N,1:N,1:M), &
            fexact(1:N,1:N,1:N,1:M))

  ! Specify the independent variables
  independentVars = (/'x','y','z'/)

  ! Specify an equation string that we want to evaluate
  eqChar = 'f = cos( 2.0*pi*x )*cos( 2.0*pi*y )*cos( 2.0*pi*z )'

  ! Create the EquationParser object
  f = EquationParser(eqChar,independentVars)

  x = 0.0_real32
  do l = 1,M
    do k = 1,N
    do j = 1,N
      do i = 1,N
        x(i,j,k,l,1) = -1.0_real32 + (2.0_real32)/real(N,real32)*real(i - 1,real32) + 2.0_real32*real(l - 1,real32)
        x(i,j,k,l,2) = -1.0_real32 + (2.0_real32)/real(N,real32)*real(j - 1,real32)
        x(i,j,k,l,3) = -1.0_real32 + (2.0_real32)/real(N,real32)*real(k - 1,real32)
      end do
    end do
    end do
  end do
  do l = 1,M
    do k = 1,N
    do j = 1,N
      do i = 1,N
       fexact(i,j,k,l) = cos(2.0_real32*pi*x(i,j,k,l,1))*cos(2.0_real32*pi*x(i,j,k,l,2))*cos(2.0_real32*pi*x(i,j,k,l,3))
      end do
    end do
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

end function cos_r4fp32
end program test

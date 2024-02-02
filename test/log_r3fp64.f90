
integer function log_r3fp64() result(r)
  use FEQParse
  use iso_fortran_env
  implicit none
  integer,parameter :: N = 10
  type(EquationParser) :: f
  character(LEN=1),dimension(1:3) :: independentVars
  character(LEN=1024) :: eqChar
  real(real64),allocatable :: x(:,:,:,:)
  real(real64),allocatable :: feval(:,:,:)
  real(real64),allocatable :: fexact(:,:,:)
  integer :: i,j,k

  allocate (x(1:N,1:N,1:N,1:3), &
            feval(1:N,1:N,1:N), &
            fexact(1:N,1:N,1:N))

  ! Specify the independent variables
  independentVars = (/'x','y','z'/)

  ! Specify an equation string that we want to evaluate
  eqChar = 'f = ln( x )*ln(y)*ln(z)'

  ! Create the EquationParser object
  f = EquationParser(eqChar,independentVars)

  x = 0.0_real64
  do k = 1,N
    do j = 1,N
      do i = 1,N
        x(i,j,k,1) = 1.0_real64 + (2.0_real64)/real(N,real64)*real(i - 1,real64)
        x(i,j,k,2) = 1.0_real64 + (2.0_real64)/real(N,real64)*real(j - 1,real64)
        x(i,j,k,3) = 1.0_real64 + (2.0_real64)/real(N,real64)*real(k - 1,real64)
      end do
    end do
  end do
  do k = 1,N
    do j = 1,N
      do i = 1,N
        fexact(i,j,k) = log(x(i,j,k,1))*log(x(i,j,k,2))*log(x(i,j,k,3))
      end do
    end do
  end do

  ! Evaluate the equation
  feval = f % evaluate(x)
  if (maxval(abs(feval - fexact)) <= maxval(abs(fexact))*epsilon(1.0_real64)) then
    r = 0
  else
    r = 1
  end if

  deallocate (x,feval,fexact)

end function log_r3fp64

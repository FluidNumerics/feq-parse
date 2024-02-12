program test

  implicit none
  integer :: exit_code

  exit_code = log_r3fp32()
  stop exit_code

contains

  integer function log_r3fp32() result(r)
    use FEQParse
    use iso_fortran_env
    implicit none
    integer,parameter :: N = 10
    type(EquationParser) :: f
    character(LEN=1),dimension(1:3) :: independentVars
    character(LEN=1024) :: eqChar
    real(real32),allocatable :: x(:,:,:,:)
    real(real32),allocatable :: feval(:,:,:)
    real(real32),allocatable :: fexact(:,:,:)
    integer :: i,j,k

    allocate(x(1:N,1:N,1:N,1:3), &
             feval(1:N,1:N,1:N), &
             fexact(1:N,1:N,1:N))

    ! Specify the independent variables
    independentVars = (/'x','y','z'/)

    ! Specify an equation string that we want to evaluate
    eqChar = 'f = ln( x )*ln(y)*ln(z)'

    ! Create the EquationParser object
    f = EquationParser(eqChar,independentVars)

    x = 0.0_real32
    do k = 1,N
      do j = 1,N
        do i = 1,N
          x(i,j,k,1) = 1.0_real32+(2.0_real32)/real(N,real32)*real(i-1,real32)
          x(i,j,k,2) = 1.0_real32+(2.0_real32)/real(N,real32)*real(j-1,real32)
          x(i,j,k,3) = 1.0_real32+(2.0_real32)/real(N,real32)*real(k-1,real32)
        enddo
      enddo
    enddo
    do k = 1,N
      do j = 1,N
        do i = 1,N
          fexact(i,j,k) = log(x(i,j,k,1))*log(x(i,j,k,2))*log(x(i,j,k,3))
        enddo
      enddo
    enddo

    ! Evaluate the equation
    feval = f%evaluate(x)
    if(maxval(abs(feval-fexact)) <= maxval(abs(fexact))*epsilon(1.0_real32)) then
      r = 0
    else
      r = 1
    endif

    deallocate(x,feval,fexact)

  endfunction log_r3fp32
endprogram test

program array_with_array_eval
  use iso_fortran_env
  use FEQParse

  implicit none
  integer,parameter :: N = 10000
  type(EquationParser) :: f
  character(LEN=1),dimension(1) :: independentVars
  character(LEN=30) :: eqChar
  real :: x(1:N,1)
  real :: feval(1:N)
  integer :: i
  real :: t1,t2

  ! Specify the independent variables
  independentVars = (/'x'/)

  ! Specify an equation string that we want to evaluate
  eqChar = 'f = exp( -(x^2) )'

  ! Create the EquationParser object
  f = EquationParser(eqChar,independentVars)

  ! Evaluate the equation
  call cpu_time(t1)
  do i = 1,N
    x(i,1) = -1.0_real32+(2.0_real32)/real(N,real32)*real(i-1,real32)
  enddo
  feval = f%evaluate(x)
  call cpu_time(t2)
  print*,"runtime :",(t2-t1)," s"

endprogram array_with_array_eval

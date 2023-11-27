
INTEGER FUNCTION log_r1fp32() RESULT(r)
  USE FEQParse
  use iso_fortran_env
  IMPLICIT NONE
  integer, parameter :: N=1000
  TYPE(EquationParser) :: f
  CHARACTER(LEN=1), DIMENSION(1:3) :: independentVars
  CHARACTER(LEN=30) :: eqChar
  REAL(real32) :: x(1:N,1:3)
  REAL(real32) :: feval(1:N)
  REAL(real32) :: fexact(1:N)
  integer :: i

    ! Specify the independent variables
    independentVars = (/ 'x', 'y', 'z' /)

    ! Specify an equation string that we want to evaluate 
    eqChar = 'f = \ln( x )'

    ! Create the EquationParser object
    f = EquationParser(eqChar, independentVars)
   
    x = 0.0_real32
    do i = 1,N
      x(i,1) = 1.0_real32 + (2.0_real32)/REAL(N,real32)*REAL(i-1,real32)
      fexact(i) = log(x(i,1))
    enddo

    ! Evaluate the equation 
    feval = f % evaluate( x )
    IF( MAXVAL(ABS(feval-fexact)) <= epsilon(1.0_real32) )THEN
      r = 0
    ELSE
      r = 1
    ENDIF

    ! Clean up memory
    CALL f % Destruct()

END FUNCTION log_r1fp32

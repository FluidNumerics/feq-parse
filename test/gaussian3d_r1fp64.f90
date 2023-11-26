
INTEGER FUNCTION gaussian3d_r1fp64() RESULT(r)
  USE FEQParse
  use iso_fortran_env
  IMPLICIT NONE
  integer, parameter :: N=1000
  TYPE(EquationParser) :: f
  CHARACTER(LEN=1), DIMENSION(1:3) :: independentVars
  CHARACTER(LEN=30) :: eqChar
  REAL(real64) :: x(1:N,1:3)
  REAL(real64) :: feval(1:N)
  REAL(real64) :: fexact(1:N)
  integer :: i

    ! Specify the independent variables
    independentVars = (/ 'x', 'y', 'z' /)

    ! Specify an equation string that we want to evaluate 
    eqChar = 'f = \exp( -(x^2 + y^2 + z^2) )'

    ! Create the EquationParser object
    f = EquationParser(eqChar, independentVars)
   
    x = 0.0_real64
    do i = 1,N
      x(i,1) = -1.0_real64 + (2.0_real64)/REAL(N,real64)*REAL(i-1,real64)
      x(i,2) = -1.0_real64 + (2.0_real64)/REAL(N,real64)*REAL(i-1,real64)
      x(i,3) = -1.0_real64 + (2.0_real64)/REAL(N,real64)*REAL(i-1,real64)
      fexact(i) = exp( -(x(i,1)**2 + x(i,2)**2 + x(i,3)**2) )
    enddo

    ! Evaluate the equation 
    feval = f % evaluate( x )
    IF( MAXVAL(ABS(feval-fexact)) <= epsilon(1.0_real64) )THEN
      r = 0
    ELSE
      r = 1
    ENDIF

    ! Clean up memory
    CALL f % Destruct()

END FUNCTION gaussian3d_r1fp64

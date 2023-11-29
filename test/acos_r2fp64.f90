
INTEGER FUNCTION acos_r2fp64() RESULT(r)
  USE FEQParse
  use iso_fortran_env
  IMPLICIT NONE
  integer, parameter :: N=1000
  TYPE(EquationParser) :: f
  CHARACTER(LEN=1), DIMENSION(1:3) :: independentVars
  CHARACTER(LEN=30) :: eqChar
  REAL(real64) :: x(1:N,1:N,1:3)
  REAL(real64) :: feval(1:N,1:N)
  REAL(real64) :: fexact(1:N,1:N)
  integer :: i,j

    ! Specify the independent variables
    independentVars = (/ 'x', 'y', 'z' /)

    ! Specify an equation string that we want to evaluate 
    eqChar = 'f = \acos( x )'

    ! Create the EquationParser object
    f = EquationParser(eqChar, independentVars)
   
    x = 0.0_real64
    do j = 1,N
      do i = 1,N
        x(i,j,1) = -1.0_real32 + (2.0_real32)/REAL(N,real32)*REAL(i-1,real32)
        x(i,j,2) = -1.0_real32 + (2.0_real32)/REAL(N,real32)*REAL(j-1,real32)
      fexact(i,j) = acos(x(i,j,1))
      enddo
    enddo

    ! Evaluate the equation 
    feval = f % evaluate( x )
    IF( MAXVAL(ABS(feval-fexact)) <= epsilon(1.0) )THEN
      r = 0
    ELSE
      r = 1
    ENDIF

    ! Clean up memory
    CALL f % Destruct()

END FUNCTION acos_r2fp64

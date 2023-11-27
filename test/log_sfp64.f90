
INTEGER FUNCTION log_sfp64() RESULT(r)
  USE FEQParse
  use iso_fortran_env
  IMPLICIT NONE
  integer, parameter :: N=1000
  TYPE(EquationParser) :: f
  CHARACTER(LEN=1), DIMENSION(1:3) :: independentVars
  CHARACTER(LEN=30) :: eqChar
  REAL(real64) :: x(1:3)
  REAL(real64) :: feval
  REAL(real64) :: fexact
  integer :: i

    ! Specify the independent variables
    independentVars = (/ 'x', 'y', 'z' /)

    ! Specify an equation string that we want to evaluate 
    eqChar = 'f = \ln( x )'

    ! Create the EquationParser object
    f = EquationParser(eqChar, independentVars)
   
    x = 1.0_real64
    fexact = log(x(1))

    ! Evaluate the equation 
    feval = f % evaluate( x )
    IF( (ABS(feval-fexact)) <= epsilon(1.0_real64) )THEN
      r = 0
    ELSE
      r = 1
    ENDIF

    ! Clean up memory
    CALL f % Destruct()

END FUNCTION log_sfp64

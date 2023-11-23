
INTEGER FUNCTION gaussian3d_sfp64() RESULT(r)
  USE FEQParse
  use iso_fortran_env
  IMPLICIT NONE

  TYPE(EquationParser) :: f
  CHARACTER(LEN=1), DIMENSION(1:3) :: independentVars
  CHARACTER(LEN=30) :: eqChar
  REAL(real64) :: x(1:3)

    ! Specify the independent variables
    independentVars = (/ 'x', 'y', 'z' /)

    ! Specify an equation string that we want to evaluate 
    eqChar = 'f = \exp( -(x^2 + y^2 + z^2) )'

    ! Create the EquationParser object
    f = EquationParser(eqChar, independentVars)
   
    ! Evaluate the equation 
    x = (/ 0.0, 0.0, 0.0 /) 
    IF( ABS(f % evaluate( x ) - 1.0) <= epsilon(1.0) )THEN
      r = 0
    ELSE
      r = 1
    ENDIF

    ! Clean up memory
    CALL f % Destruct()

END FUNCTION gaussian3d_sfp64

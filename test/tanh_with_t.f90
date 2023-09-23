
INTEGER FUNCTION tanh_with_t() RESULT(r)
  USE FEQParse
  USE ISO_FORTRAN_ENV
  IMPLICIT NONE

  TYPE(EquationParser) :: f
  CHARACTER(LEN=1), DIMENSION(1:2) :: independentVars
  CHARACTER(LEN=100) :: eqChar
  REAL(real64) :: x(1:2)
  REAL(real64) :: exact

    ! Specify the independent variables
    independentVars = (/ 'x', 't' /)

    ! Evaluate directly
    exact = 0.5*(1.0 - tanh((0.0 - 250000.0 - 0.5*1000000.0)/100000.0))
    ! Specify an equation string that we want to evaluate 
    eqChar = "f = 0.5*(1.0 - \tanh((x - (250000 + 0.5*t))/100000))"   ! Create the EquationParser object
    f = EquationParser(eqChar, independentVars)

    ! Evaluate the equation at (0,1000)
    x = (/ 0.0, 1000000.0 /)
    IF( ABS(f % evaluate( x ) - exact) <= epsilon(exact) )THEN
      r = 0
    ELSE
      r = 1
    ENDIF

    ! Clean up memory
    CALL f % Destruct()

END FUNCTION tanh_with_t

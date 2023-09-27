
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
    exact = 0.5_real64*(1.0_real64 - tanh((0.0_real64 - 250000.0_real64 - 0.5_real64*1000000.0_real64)/100000.0_real64))
    ! Specify an equation string that we want to evaluate 
    eqChar = "f = 0.5*(1.0 - \tanh((x - (250000.0 + 0.5*t))/100000.0))"   ! Create the EquationParser object
    f = EquationParser(eqChar, independentVars)
    PRINT*, ABS(f % evaluate( x ) - exact),f % evaluate( x ), exact, epsilon(exact)
    ! Evaluate the equation at (0,1000)
    x = (/ 0.0_real64, 1000000.0_real64 /)
    IF( ABS(f % evaluate( x ) - exact) <= 10.0_real64**(-3) )THEN
      r = 0
    ELSE
      r = 1
    ENDIF

    ! Clean up memory
    CALL f % Destruct()

END FUNCTION tanh_with_t

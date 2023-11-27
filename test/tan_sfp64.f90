
INTEGER FUNCTION tan_sfp64() RESULT(r)
  USE FEQParse
  use iso_fortran_env
  IMPLICIT NONE
  integer, parameter :: N=1000
  real(real64), parameter :: pi = 4.0_real64*atan(1.0_real64)
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
    eqChar = 'f = \tan( 0.5*pi*x )'

    ! Create the EquationParser object
    f = EquationParser(eqChar, independentVars)
   
    x = 0.0_real64
    fexact = tan(0.5_real64*pi*x(1))

    ! Evaluate the equation 
    feval = f % evaluate( x )
    IF( (ABS(feval-fexact)) <= epsilon(1.0_real64) )THEN
      r = 0
    ELSE
      r = 1
    ENDIF

    ! Clean up memory
    CALL f % Destruct()

END FUNCTION tan_sfp64
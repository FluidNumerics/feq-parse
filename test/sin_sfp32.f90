
INTEGER FUNCTION sin_sfp32() RESULT(r)
  USE FEQParse
  use iso_fortran_env
  IMPLICIT NONE
  integer, parameter :: N=1000
  real(real32), parameter :: pi = 4.0_real32*atan(1.0_real32)
  TYPE(EquationParser) :: f
  CHARACTER(LEN=1), DIMENSION(1:3) :: independentVars
  CHARACTER(LEN=2048) :: eqChar
  REAL(real32) :: x(1:3)
  REAL(real32) :: feval
  REAL(real32) :: fexact
  integer :: i

    ! Specify the independent variables
    independentVars = (/ 'x', 'y', 'z' /)

    ! Specify an equation string that we want to evaluate 
    eqChar = 'f = \sin( 2.0*pi*x )'

    ! Create the EquationParser object
    f = EquationParser(eqChar, independentVars)
   
    x = 0.0_real32
    fexact = sin(2.0_real32*pi*x(1))

    ! Evaluate the equation 
    feval = f % evaluate( x )
    IF( (ABS(feval-fexact)) <= epsilon(1.0_real32) )THEN
      r = 0
    ELSE
      r = 1
    ENDIF

    ! Clean up memory
    CALL f % Destruct()

END FUNCTION sin_sfp32

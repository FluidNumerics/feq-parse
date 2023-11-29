
INTEGER FUNCTION linear_r2fp32() RESULT(r)
  ! WARNING : the change in order of operations with feq-parse compared to the compilers
  ! implementation for "fexact" leads to differences that are larger than machine epsilon.
  USE FEQParse
  use iso_fortran_env
  IMPLICIT NONE
  integer, parameter :: N=1000
  TYPE(EquationParser) :: f
  CHARACTER(LEN=1), DIMENSION(1:3) :: independentVars
  CHARACTER(LEN=30) :: eqChar
  REAL(real32) :: x(1:N,1:N,1:3)
  REAL(real32) :: feval(1:N,1:N)
  REAL(real32) :: fexact(1:N,1:N)
  integer :: i,j

    ! Specify the independent variables
    independentVars = (/ 'x', 'y', 'z' /)

    ! Specify an equation string that we want to evaluate 
    eqChar = 'f = (x^3-1)*(y^3-1)'

    ! Create the EquationParser object
    f = EquationParser(eqChar, independentVars)
   
    x = 0.0_real32
    do j = 1,N
      do i = 1,N
        x(i,j,1) = -1.0_real32 + (2.0_real32)/REAL(N,real32)*REAL(i-1,real32)
        x(i,j,2) = -1.0_real32 + (2.0_real32)/REAL(N,real32)*REAL(j-1,real32)
      fexact(i,j) = (x(i,j,1)**3 -1.0_real32)*(x(i,j,2)**3 -1.0_real32)
      enddo
    enddo

    ! Evaluate the equation 
    feval = f % evaluate( x )
    IF( MAXVAL(ABS(feval-fexact)) <= 10.0_real32*epsilon(1.0_real32) )THEN
      r = 0
    ELSE
      r = 1
    ENDIF

    ! Clean up memory
    CALL f % Destruct()

END FUNCTION linear_r2fp32

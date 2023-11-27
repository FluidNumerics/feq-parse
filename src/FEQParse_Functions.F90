! FEQParse.F03
! 
! Copyright 2020 Fluid Numerics LLC
! All rights reserved.
!
! Author : Joe Schoonover ( joe@fluidnumerics.com )
!
! EquationParser defines a public class that can be used to parse and evaluate strings
! representative of equations. An equation, written in infix form, is converted to
! postfix form and evaluated using a postfix calculator.
!
! //////////////////////////////////////////////////////////////////////////////////////////////// !

module FEQParse_Functions

use ISO_FORTRAN_ENV

implicit none

  integer, parameter, private :: nFunctions_default = 14
  integer, parameter, public :: feqparse_function_maxlength = 10

  type FEQParse_Function
    character(feqparse_function_maxlength) :: str
  end type FEQParse_Function

  type FEQParse_FunctionHandler
    integer :: nfunctions
    type(FEQParse_Function), allocatable :: functions(:)

    contains
    procedure, public :: Destruct => Destruct_FEQParse_FunctionHandler
    procedure, public :: IsFunction
    generic, public :: f_of_x => f_of_x_sfp32, &
                                 f_of_x_r1fp32, &
                                 f_of_x_sfp64, &
                                 f_of_x_r1fp64

    procedure, private :: f_of_x_sfp32
    procedure, private :: f_of_x_r1fp32
    procedure, private :: f_of_x_sfp64
    procedure, private :: f_of_x_r1fp64

  end type FEQParse_FunctionHandler

  
  interface FEQParse_FunctionHandler
  procedure Construct_FEQParse_FunctionHandler
  end interface FEQParse_FunctionHandler

contains

  function Construct_FEQParse_FunctionHandler() result( functionhandler_obj )
    type(FEQParse_FunctionHandler) :: functionhandler_obj

    functionhandler_obj % nfunctions = nFunctions_default
    allocate( functionhandler_obj % functions(1:nFunctions_default))
    functionhandler_obj % functions(1) % str = "\cos"
    functionhandler_obj % functions(2) % str = "\sin"
    functionhandler_obj % functions(3) % str = "\tan"
    functionhandler_obj % functions(4) % str = "\tanh"
    functionhandler_obj % functions(5) % str = "\sqrt"
    functionhandler_obj % functions(6) % str = "\abs"
    functionhandler_obj % functions(7) % str = "\exp"
    functionhandler_obj % functions(8) % str = "\ln"
    functionhandler_obj % functions(9) % str = "\log"
    functionhandler_obj % functions(10) % str = "\acos"
    functionhandler_obj % functions(11) % str = "\asin"
    functionhandler_obj % functions(12) % str = "\atan"
    functionhandler_obj % functions(13) % str = "\sech"
    functionhandler_obj % functions(14) % str = "\rand"

  end function Construct_FEQParse_FunctionHandler

  subroutine Destruct_FEQParse_FunctionHandler( functionhandler_obj )
    class(FEQParse_FunctionHandler), intent(inout) :: functionhandler_obj

    deallocate( functionhandler_obj % functions )
    functionhandler_obj % nfunctions = 0

  end subroutine Destruct_FEQParse_FunctionHandler


  LOGICAL FUNCTION IsFunction( functionhandler_obj, eqChar )
    class(FEQParse_FunctionHandler) :: functionhandler_obj
    CHARACTER(*) :: eqChar
    ! Local
    INTEGER :: i

      IsFunction = .FALSE.
      DO i = 1, functionhandler_obj %nFunctions

        IF( eqChar(1:1) == "\" ) THEN
          IsFunction = .TRUE.
        ENDIF

      ENDDO

  END FUNCTION IsFunction

  FUNCTION FindLastFunctionIndex( eqChar ) RESULT( j )
    CHARACTER(*) :: eqChar
    INTEGER      :: i, j

      DO i = 1, LEN(eqChar)
        IF( eqChar(i:i) == "(" )THEN
          j = i-2
          EXIT
        ENDIF

      ENDDO
         
  END FUNCTION FindLastFunctionIndex

  subroutine f_of_x_sfp32( functionhandler_obj, func, x, fx )
    !! Evaluates function for scalar fp32 input and output
    class(FEQParse_FunctionHandler) :: functionhandler_obj
    CHARACTER(*), intent(in) :: func
    REAL(real32), intent(in) :: x
    real(real32), intent(out) :: fx
    ! Local
    REAL(real32)   :: r

      IF( TRIM( func ) == "\cos" .OR. TRIM( func ) == "\COS" )THEN

        fx = cos( x )

      ELSEIF( TRIM( func ) == "\sin" .OR. TRIM( func ) == "\SIN" )THEN

        fx = sin( x )

      ELSEIF( TRIM( func ) == "\tan" .OR. TRIM( func ) == "\TAN" )THEN

        fx = tan( x )

      ELSEIF( TRIM( func ) == "\tanh" .OR. TRIM( func ) == "\TANH" )THEN

        fx = tanh( x )

      ELSEIF( TRIM( func ) == "\sech" .OR. TRIM( func ) == "\SECH" )THEN

        fx = 2.0_real32/( exp(x) + exp(-x) )

      ELSEIF( TRIM( func ) == "\sqrt" .OR. TRIM( func ) == "\SQRT" )THEN

        fx = sqrt( x )

      ELSEIF( TRIM( func ) == "\abs" .OR. TRIM( func ) == "\ABS" )THEN

        fx = abs( x )

      ELSEIF( TRIM( func ) == "\exp" .OR. TRIM( func ) == "\EXP" )THEN

        fx = exp( x )

      ELSEIF( TRIM( func ) == "\ln" .OR. TRIM( func ) == "\LN" )THEN

        fx = log( x )

      ELSEIF( TRIM( func ) == "\log" .OR. TRIM( func ) == "\LOG" )THEN

        fx = log10( x )

      ELSEIF( TRIM( func ) == "\acos" .OR. TRIM( func ) == "\ACOS" )THEN

        fx = acos( x )

      ELSEIF( TRIM( func ) == "\asin" .OR. TRIM( func ) == "\ASIN" )THEN

        fx = asin( x )

      ELSEIF( TRIM( func ) == "\atan" .OR. TRIM( func ) == "\ATAN" )THEN

        fx = atan( x )

      ELSEIF( TRIM( func ) == "\rand" .OR. TRIM( func ) == "\RAND" )THEN

        CALL RANDOM_NUMBER( r )
        fx = r*x

      ELSE
 
        fx = 0.0_real32

      ENDIF


  end subroutine f_of_x_sfp32

  subroutine f_of_x_r1fp32( functionhandler_obj, func, x, fx )
    !! Evaluates function for rank-1 array fp32 input and output
    class(FEQParse_FunctionHandler) :: functionhandler_obj
    CHARACTER(*), intent(in)  :: func
    REAL(real32), intent(in)  :: x(:)
    real(real32), intent(out) :: fx(:)
    ! Local
    REAL(real32)   :: r
    integer :: i

      IF( TRIM( func ) == "\cos" .OR. TRIM( func ) == "\COS" )THEN
        
        fx = cos( x )

      ELSEIF( TRIM( func ) == "\sin" .OR. TRIM( func ) == "\SIN" )THEN

        fx = sin( x )

      ELSEIF( TRIM( func ) == "\tan" .OR. TRIM( func ) == "\TAN" )THEN

        fx = tan( x )

      ELSEIF( TRIM( func ) == "\tanh" .OR. TRIM( func ) == "\TANH" )THEN

        fx = tanh( x )

      ELSEIF( TRIM( func ) == "\sech" .OR. TRIM( func ) == "\SECH" )THEN

        fx = 2.0_real32/( exp(x) + exp(-x) )

      ELSEIF( TRIM( func ) == "\sqrt" .OR. TRIM( func ) == "\SQRT" )THEN

        fx = sqrt( x )

      ELSEIF( TRIM( func ) == "\abs" .OR. TRIM( func ) == "\ABS" )THEN

        fx = abs( x )

      ELSEIF( TRIM( func ) == "\exp" .OR. TRIM( func ) == "\EXP" )THEN

        fx = exp( x )

      ELSEIF( TRIM( func ) == "\ln" .OR. TRIM( func ) == "\LN" )THEN

        fx = log( x )

      ELSEIF( TRIM( func ) == "\log" .OR. TRIM( func ) == "\LOG" )THEN

        fx = log10( x )

      ELSEIF( TRIM( func ) == "\acos" .OR. TRIM( func ) == "\ACOS" )THEN

        fx = acos( x )

      ELSEIF( TRIM( func ) == "\asin" .OR. TRIM( func ) == "\ASIN" )THEN

        fx = asin( x )

      ELSEIF( TRIM( func ) == "\atan" .OR. TRIM( func ) == "\ATAN" )THEN

        fx = atan( x )

      ELSEIF( TRIM( func ) == "\rand" .OR. TRIM( func ) == "\RAND" )THEN

        CALL RANDOM_NUMBER( r )
        fx = r*x

      ELSE
 
        fx = 0.0_real32

      ENDIF

  end subroutine f_of_x_r1fp32

  subroutine f_of_x_sfp64( functionhandler_obj, func, x, fx )
  !! Evaluates function for scalar fp64 input and output
    class(FEQParse_FunctionHandler) :: functionhandler_obj
  CHARACTER(*), intent(in) :: func
  REAL(real64), intent(in)  :: x
  real(real64), intent(out) :: fx
  ! Local
  REAL(real64)   :: r

    IF( TRIM( func ) == "\cos" .OR. TRIM( func ) == "\COS" )THEN

      fx = cos( x )

    ELSEIF( TRIM( func ) == "\sin" .OR. TRIM( func ) == "\SIN" )THEN

      fx = sin( x )

    ELSEIF( TRIM( func ) == "\tan" .OR. TRIM( func ) == "\TAN" )THEN

      fx = tan( x )

    ELSEIF( TRIM( func ) == "\tanh" .OR. TRIM( func ) == "\TANH" )THEN

      fx = tanh( x )

    ELSEIF( TRIM( func ) == "\sech" .OR. TRIM( func ) == "\SECH" )THEN

      fx = 2.0_real64/( exp(x) + exp(-x) )

    ELSEIF( TRIM( func ) == "\sqrt" .OR. TRIM( func ) == "\SQRT" )THEN

      fx = sqrt( x )

    ELSEIF( TRIM( func ) == "\abs" .OR. TRIM( func ) == "\ABS" )THEN

      fx = abs( x )

    ELSEIF( TRIM( func ) == "\exp" .OR. TRIM( func ) == "\EXP" )THEN

      fx = exp( x )

    ELSEIF( TRIM( func ) == "\ln" .OR. TRIM( func ) == "\LN" )THEN

      fx = log( x )

    ELSEIF( TRIM( func ) == "\log" .OR. TRIM( func ) == "\LOG" )THEN

      fx = log10( x )

    ELSEIF( TRIM( func ) == "\acos" .OR. TRIM( func ) == "\ACOS" )THEN

      fx = acos( x )

    ELSEIF( TRIM( func ) == "\asin" .OR. TRIM( func ) == "\ASIN" )THEN

      fx = asin( x )

    ELSEIF( TRIM( func ) == "\atan" .OR. TRIM( func ) == "\ATAN" )THEN

      fx = atan( x )

    ELSEIF( TRIM( func ) == "\rand" .OR. TRIM( func ) == "\RAND" )THEN

      CALL RANDOM_NUMBER( r )
      fx = r*x

    ELSE

      fx = 0.0_real64

    ENDIF


  end subroutine f_of_x_sfp64

  subroutine f_of_x_r1fp64( functionhandler_obj, func, x, fx )
    !! Evaluates function for scalar fp64 input and output
    class(FEQParse_FunctionHandler) :: functionhandler_obj
    CHARACTER(*), intent(in) :: func
    REAL(real64), intent(in)  :: x(:)
    real(real64), intent(out) :: fx(:)
    ! Local
    REAL(real64)   :: r
  
      IF( TRIM( func ) == "\cos" .OR. TRIM( func ) == "\COS" )THEN
  
        fx = cos( x )
  
      ELSEIF( TRIM( func ) == "\sin" .OR. TRIM( func ) == "\SIN" )THEN
  
        fx = sin( x )
  
      ELSEIF( TRIM( func ) == "\tan" .OR. TRIM( func ) == "\TAN" )THEN
  
        fx = tan( x )
  
      ELSEIF( TRIM( func ) == "\tanh" .OR. TRIM( func ) == "\TANH" )THEN
  
        fx = tanh( x )
  
      ELSEIF( TRIM( func ) == "\sech" .OR. TRIM( func ) == "\SECH" )THEN
  
        fx = 2.0_real64/( exp(x) + exp(-x) )
  
      ELSEIF( TRIM( func ) == "\sqrt" .OR. TRIM( func ) == "\SQRT" )THEN
  
        fx = sqrt( x )
  
      ELSEIF( TRIM( func ) == "\abs" .OR. TRIM( func ) == "\ABS" )THEN
  
        fx = abs( x )
  
      ELSEIF( TRIM( func ) == "\exp" .OR. TRIM( func ) == "\EXP" )THEN
  
        fx = exp( x )
  
      ELSEIF( TRIM( func ) == "\ln" .OR. TRIM( func ) == "\LN" )THEN
  
        fx = log( x )
  
      ELSEIF( TRIM( func ) == "\log" .OR. TRIM( func ) == "\LOG" )THEN
  
        fx = log10( x )
  
      ELSEIF( TRIM( func ) == "\acos" .OR. TRIM( func ) == "\ACOS" )THEN
  
        fx = acos( x )
  
      ELSEIF( TRIM( func ) == "\asin" .OR. TRIM( func ) == "\ASIN" )THEN
  
        fx = asin( x )
  
      ELSEIF( TRIM( func ) == "\atan" .OR. TRIM( func ) == "\ATAN" )THEN
  
        fx = atan( x )
  
      ELSEIF( TRIM( func ) == "\rand" .OR. TRIM( func ) == "\RAND" )THEN
  
        CALL RANDOM_NUMBER( r )
        fx = r*x
  
      ELSE
  
        fx = 0.0_real64
  
      ENDIF
  
  
    end subroutine f_of_x_r1fp64

END MODULE FEQParse_Functions

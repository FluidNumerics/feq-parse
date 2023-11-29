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

  use iso_fortran_env

  implicit none

  integer,parameter,private :: nFunctions_default = 14
  integer,parameter,public :: feqparse_function_maxlength = 10

  type FEQParse_Function
    character(feqparse_function_maxlength) :: str
  end type FEQParse_Function

  type FEQParse_FunctionHandler
    integer :: nfunctions
    type(FEQParse_Function),allocatable :: functions(:)

  contains
    procedure,public :: Destruct => Destruct_FEQParse_FunctionHandler
    procedure,public :: IsFunction
    generic,public :: f_of_x => f_of_x_sfp32, &
      f_of_x_r1fp32, &
      f_of_x_r2fp32, &
      f_of_x_r3fp32, &
      f_of_x_r4fp32,&
      f_of_x_sfp64, &
      f_of_x_r1fp64, &
      f_of_x_r2fp64, &
      f_of_x_r3fp64, &
      f_of_x_r4fp64


    procedure,private :: f_of_x_sfp32
    procedure,private :: f_of_x_r1fp32
    procedure,private :: f_of_x_r2fp32
    procedure,private :: f_of_x_r3fp32
    procedure,private :: f_of_x_r4fp32
    procedure,private :: f_of_x_sfp64
    procedure,private :: f_of_x_r1fp64
    procedure,private :: f_of_x_r2fp64
    procedure,private :: f_of_x_r3fp64
    procedure,private :: f_of_x_r4fp64

  end type FEQParse_FunctionHandler

  interface FEQParse_FunctionHandler
    procedure Construct_FEQParse_FunctionHandler
  end interface FEQParse_FunctionHandler

contains

  function Construct_FEQParse_FunctionHandler() result(functionhandler_obj)
    type(FEQParse_FunctionHandler) :: functionhandler_obj

    functionhandler_obj % nfunctions = nFunctions_default
    allocate (functionhandler_obj % functions(1:nFunctions_default))
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

  subroutine Destruct_FEQParse_FunctionHandler(functionhandler_obj)
    class(FEQParse_FunctionHandler),intent(inout) :: functionhandler_obj

    deallocate (functionhandler_obj % functions)
    functionhandler_obj % nfunctions = 0

  end subroutine Destruct_FEQParse_FunctionHandler

  logical function IsFunction(functionhandler_obj,eqChar)
    class(FEQParse_FunctionHandler) :: functionhandler_obj
    character(*) :: eqChar
    ! Local
    integer :: i

    IsFunction = .false.
    do i = 1,functionhandler_obj % nFunctions

      if (eqChar(1:1) == "\") then
        IsFunction = .true.
      end if

    end do

  end function IsFunction

  function FindLastFunctionIndex(eqChar) result(j)
    character(*) :: eqChar
    integer      :: i,j

    do i = 1,len(eqChar)
      if (eqChar(i:i) == "(") then
        j = i - 2
        exit
      end if

    end do

  end function FindLastFunctionIndex

  subroutine f_of_x_sfp32(functionhandler_obj,func,x,fx)
    !! Evaluates function for scalar fp32 input and output
    class(FEQParse_FunctionHandler) :: functionhandler_obj
    character(*),intent(in) :: func
    real(real32),intent(in) :: x
    real(real32),intent(out) :: fx
    ! Local
    real(real32)   :: r

    if (trim(func) == "\cos" .or. trim(func) == "\COS") then

      fx = cos(x)

    elseif (trim(func) == "\sin" .or. trim(func) == "\SIN") then

      fx = sin(x)

    elseif (trim(func) == "\tan" .or. trim(func) == "\TAN") then

      fx = tan(x)

    elseif (trim(func) == "\tanh" .or. trim(func) == "\TANH") then

      fx = tanh(x)

    elseif (trim(func) == "\sech" .or. trim(func) == "\SECH") then

      fx = 2.0_real32/(exp(x) + exp(-x))

    elseif (trim(func) == "\sqrt" .or. trim(func) == "\SQRT") then

      fx = sqrt(x)

    elseif (trim(func) == "\abs" .or. trim(func) == "\ABS") then

      fx = abs(x)

    elseif (trim(func) == "\exp" .or. trim(func) == "\EXP") then

      fx = exp(x)

    elseif (trim(func) == "\ln" .or. trim(func) == "\LN") then

      fx = log(x)

    elseif (trim(func) == "\log" .or. trim(func) == "\LOG") then

      fx = log10(x)

    elseif (trim(func) == "\acos" .or. trim(func) == "\ACOS") then

      fx = acos(x)

    elseif (trim(func) == "\asin" .or. trim(func) == "\ASIN") then

      fx = asin(x)

    elseif (trim(func) == "\atan" .or. trim(func) == "\ATAN") then

      fx = atan(x)

    elseif (trim(func) == "\rand" .or. trim(func) == "\RAND") then

      call random_number(r)
      fx = r*x

    else

      fx = 0.0_real32

    end if

  end subroutine f_of_x_sfp32

  subroutine f_of_x_r1fp32(functionhandler_obj,func,x,fx)
    !! Evaluates function for rank-1 array fp32 input and output
    class(FEQParse_FunctionHandler) :: functionhandler_obj
    character(*),intent(in)  :: func
    real(real32),intent(in)  :: x(:)
    real(real32),intent(out) :: fx(:)
    ! Local
    real(real32)   :: r
    integer :: i

    if (trim(func) == "\cos" .or. trim(func) == "\COS") then

      fx = cos(x)

    elseif (trim(func) == "\sin" .or. trim(func) == "\SIN") then

      fx = sin(x)

    elseif (trim(func) == "\tan" .or. trim(func) == "\TAN") then

      fx = tan(x)

    elseif (trim(func) == "\tanh" .or. trim(func) == "\TANH") then

      fx = tanh(x)

    elseif (trim(func) == "\sech" .or. trim(func) == "\SECH") then

      fx = 2.0_real32/(exp(x) + exp(-x))

    elseif (trim(func) == "\sqrt" .or. trim(func) == "\SQRT") then

      fx = sqrt(x)

    elseif (trim(func) == "\abs" .or. trim(func) == "\ABS") then

      fx = abs(x)

    elseif (trim(func) == "\exp" .or. trim(func) == "\EXP") then

      fx = exp(x)

    elseif (trim(func) == "\ln" .or. trim(func) == "\LN") then

      fx = log(x)

    elseif (trim(func) == "\log" .or. trim(func) == "\LOG") then

      fx = log10(x)

    elseif (trim(func) == "\acos" .or. trim(func) == "\ACOS") then

      fx = acos(x)

    elseif (trim(func) == "\asin" .or. trim(func) == "\ASIN") then

      fx = asin(x)

    elseif (trim(func) == "\atan" .or. trim(func) == "\ATAN") then

      fx = atan(x)

    elseif (trim(func) == "\rand" .or. trim(func) == "\RAND") then

      call random_number(r)
      fx = r*x

    else

      fx = 0.0_real32

    end if

  end subroutine f_of_x_r1fp32

  subroutine f_of_x_r2fp32(functionhandler_obj,func,x,fx)
    !! Evaluates function for rank-2 array fp32 input and output
    class(FEQParse_FunctionHandler) :: functionhandler_obj
    character(*),intent(in)  :: func
    real(real32),intent(in)  :: x(:,:)
    real(real32),intent(out) :: fx(:,:)
    ! Local
    real(real32)   :: r
    integer :: i

    if (trim(func) == "\cos" .or. trim(func) == "\COS") then

      fx = cos(x)

    elseif (trim(func) == "\sin" .or. trim(func) == "\SIN") then

      fx = sin(x)

    elseif (trim(func) == "\tan" .or. trim(func) == "\TAN") then

      fx = tan(x)

    elseif (trim(func) == "\tanh" .or. trim(func) == "\TANH") then

      fx = tanh(x)

    elseif (trim(func) == "\sech" .or. trim(func) == "\SECH") then

      fx = 2.0_real32/(exp(x) + exp(-x))

    elseif (trim(func) == "\sqrt" .or. trim(func) == "\SQRT") then

      fx = sqrt(x)

    elseif (trim(func) == "\abs" .or. trim(func) == "\ABS") then

      fx = abs(x)

    elseif (trim(func) == "\exp" .or. trim(func) == "\EXP") then

      fx = exp(x)

    elseif (trim(func) == "\ln" .or. trim(func) == "\LN") then

      fx = log(x)

    elseif (trim(func) == "\log" .or. trim(func) == "\LOG") then

      fx = log10(x)

    elseif (trim(func) == "\acos" .or. trim(func) == "\ACOS") then

      fx = acos(x)

    elseif (trim(func) == "\asin" .or. trim(func) == "\ASIN") then

      fx = asin(x)

    elseif (trim(func) == "\atan" .or. trim(func) == "\ATAN") then

      fx = atan(x)

    elseif (trim(func) == "\rand" .or. trim(func) == "\RAND") then

      call random_number(r)
      fx = r*x

    else

      fx = 0.0_real32

    end if

  end subroutine f_of_x_r2fp32

  subroutine f_of_x_r3fp32(functionhandler_obj,func,x,fx)
    !! Evaluates function for rank-3 array fp32 input and output
    class(FEQParse_FunctionHandler) :: functionhandler_obj
    character(*),intent(in)  :: func
    real(real32),intent(in)  :: x(:,:,:)
    real(real32),intent(out) :: fx(:,:,:)
    ! Local
    real(real32)   :: r
    integer :: i

    if (trim(func) == "\cos" .or. trim(func) == "\COS") then

      fx = cos(x)

    elseif (trim(func) == "\sin" .or. trim(func) == "\SIN") then

      fx = sin(x)

    elseif (trim(func) == "\tan" .or. trim(func) == "\TAN") then

      fx = tan(x)

    elseif (trim(func) == "\tanh" .or. trim(func) == "\TANH") then

      fx = tanh(x)

    elseif (trim(func) == "\sech" .or. trim(func) == "\SECH") then

      fx = 2.0_real32/(exp(x) + exp(-x))

    elseif (trim(func) == "\sqrt" .or. trim(func) == "\SQRT") then

      fx = sqrt(x)

    elseif (trim(func) == "\abs" .or. trim(func) == "\ABS") then

      fx = abs(x)

    elseif (trim(func) == "\exp" .or. trim(func) == "\EXP") then

      fx = exp(x)

    elseif (trim(func) == "\ln" .or. trim(func) == "\LN") then

      fx = log(x)

    elseif (trim(func) == "\log" .or. trim(func) == "\LOG") then

      fx = log10(x)

    elseif (trim(func) == "\acos" .or. trim(func) == "\ACOS") then

      fx = acos(x)

    elseif (trim(func) == "\asin" .or. trim(func) == "\ASIN") then

      fx = asin(x)

    elseif (trim(func) == "\atan" .or. trim(func) == "\ATAN") then

      fx = atan(x)

    elseif (trim(func) == "\rand" .or. trim(func) == "\RAND") then

      call random_number(r)
      fx = r*x

    else

      fx = 0.0_real32

    end if

  end subroutine f_of_x_r3fp32

  subroutine f_of_x_r4fp32(functionhandler_obj,func,x,fx)
    !! Evaluates function for rank-4 array fp32 input and output
    class(FEQParse_FunctionHandler) :: functionhandler_obj
    character(*),intent(in)  :: func
    real(real32),intent(in)  :: x(:,:,:,:)
    real(real32),intent(out) :: fx(:,:,:,:)
    ! Local
    real(real32)   :: r
    integer :: i

    if (trim(func) == "\cos" .or. trim(func) == "\COS") then

      fx = cos(x)

    elseif (trim(func) == "\sin" .or. trim(func) == "\SIN") then

      fx = sin(x)

    elseif (trim(func) == "\tan" .or. trim(func) == "\TAN") then

      fx = tan(x)

    elseif (trim(func) == "\tanh" .or. trim(func) == "\TANH") then

      fx = tanh(x)

    elseif (trim(func) == "\sech" .or. trim(func) == "\SECH") then

      fx = 2.0_real32/(exp(x) + exp(-x))

    elseif (trim(func) == "\sqrt" .or. trim(func) == "\SQRT") then

      fx = sqrt(x)

    elseif (trim(func) == "\abs" .or. trim(func) == "\ABS") then

      fx = abs(x)

    elseif (trim(func) == "\exp" .or. trim(func) == "\EXP") then

      fx = exp(x)

    elseif (trim(func) == "\ln" .or. trim(func) == "\LN") then

      fx = log(x)

    elseif (trim(func) == "\log" .or. trim(func) == "\LOG") then

      fx = log10(x)

    elseif (trim(func) == "\acos" .or. trim(func) == "\ACOS") then

      fx = acos(x)

    elseif (trim(func) == "\asin" .or. trim(func) == "\ASIN") then

      fx = asin(x)

    elseif (trim(func) == "\atan" .or. trim(func) == "\ATAN") then

      fx = atan(x)

    elseif (trim(func) == "\rand" .or. trim(func) == "\RAND") then

      call random_number(r)
      fx = r*x

    else

      fx = 0.0_real32

    end if

  end subroutine f_of_x_r4fp32

  subroutine f_of_x_sfp64(functionhandler_obj,func,x,fx)
  !! Evaluates function for scalar fp64 input and output
    class(FEQParse_FunctionHandler) :: functionhandler_obj
    character(*),intent(in) :: func
    real(real64),intent(in)  :: x
    real(real64),intent(out) :: fx
    ! Local
    real(real64)   :: r

    if (trim(func) == "\cos" .or. trim(func) == "\COS") then

      fx = cos(x)

    elseif (trim(func) == "\sin" .or. trim(func) == "\SIN") then

      fx = sin(x)

    elseif (trim(func) == "\tan" .or. trim(func) == "\TAN") then

      fx = tan(x)

    elseif (trim(func) == "\tanh" .or. trim(func) == "\TANH") then

      fx = tanh(x)

    elseif (trim(func) == "\sech" .or. trim(func) == "\SECH") then

      fx = 2.0_real64/(exp(x) + exp(-x))

    elseif (trim(func) == "\sqrt" .or. trim(func) == "\SQRT") then

      fx = sqrt(x)

    elseif (trim(func) == "\abs" .or. trim(func) == "\ABS") then

      fx = abs(x)

    elseif (trim(func) == "\exp" .or. trim(func) == "\EXP") then

      fx = exp(x)

    elseif (trim(func) == "\ln" .or. trim(func) == "\LN") then

      fx = log(x)

    elseif (trim(func) == "\log" .or. trim(func) == "\LOG") then

      fx = log10(x)

    elseif (trim(func) == "\acos" .or. trim(func) == "\ACOS") then

      fx = acos(x)

    elseif (trim(func) == "\asin" .or. trim(func) == "\ASIN") then

      fx = asin(x)

    elseif (trim(func) == "\atan" .or. trim(func) == "\ATAN") then

      fx = atan(x)

    elseif (trim(func) == "\rand" .or. trim(func) == "\RAND") then

      call random_number(r)
      fx = r*x

    else

      fx = 0.0_real64

    end if

  end subroutine f_of_x_sfp64

  subroutine f_of_x_r1fp64(functionhandler_obj,func,x,fx)
    !! Evaluates function for scalar fp64 input and output
    class(FEQParse_FunctionHandler) :: functionhandler_obj
    character(*),intent(in) :: func
    real(real64),intent(in)  :: x(:)
    real(real64),intent(out) :: fx(:)
    ! Local
    real(real64)   :: r

    if (trim(func) == "\cos" .or. trim(func) == "\COS") then

      fx = cos(x)

    elseif (trim(func) == "\sin" .or. trim(func) == "\SIN") then

      fx = sin(x)

    elseif (trim(func) == "\tan" .or. trim(func) == "\TAN") then

      fx = tan(x)

    elseif (trim(func) == "\tanh" .or. trim(func) == "\TANH") then

      fx = tanh(x)

    elseif (trim(func) == "\sech" .or. trim(func) == "\SECH") then

      fx = 2.0_real64/(exp(x) + exp(-x))

    elseif (trim(func) == "\sqrt" .or. trim(func) == "\SQRT") then

      fx = sqrt(x)

    elseif (trim(func) == "\abs" .or. trim(func) == "\ABS") then

      fx = abs(x)

    elseif (trim(func) == "\exp" .or. trim(func) == "\EXP") then

      fx = exp(x)

    elseif (trim(func) == "\ln" .or. trim(func) == "\LN") then

      fx = log(x)

    elseif (trim(func) == "\log" .or. trim(func) == "\LOG") then

      fx = log10(x)

    elseif (trim(func) == "\acos" .or. trim(func) == "\ACOS") then

      fx = acos(x)

    elseif (trim(func) == "\asin" .or. trim(func) == "\ASIN") then

      fx = asin(x)

    elseif (trim(func) == "\atan" .or. trim(func) == "\ATAN") then

      fx = atan(x)

    elseif (trim(func) == "\rand" .or. trim(func) == "\RAND") then

      call random_number(r)
      fx = r*x

    else

      fx = 0.0_real64

    end if

  end subroutine f_of_x_r1fp64

  subroutine f_of_x_r2fp64(functionhandler_obj,func,x,fx)
    !! Evaluates function for scalar fp64 input and output
    class(FEQParse_FunctionHandler) :: functionhandler_obj
    character(*),intent(in) :: func
    real(real64),intent(in)  :: x(:,:)
    real(real64),intent(out) :: fx(:,:)
    ! Local
    real(real64)   :: r

    if (trim(func) == "\cos" .or. trim(func) == "\COS") then

      fx = cos(x)

    elseif (trim(func) == "\sin" .or. trim(func) == "\SIN") then

      fx = sin(x)

    elseif (trim(func) == "\tan" .or. trim(func) == "\TAN") then

      fx = tan(x)

    elseif (trim(func) == "\tanh" .or. trim(func) == "\TANH") then

      fx = tanh(x)

    elseif (trim(func) == "\sech" .or. trim(func) == "\SECH") then

      fx = 2.0_real64/(exp(x) + exp(-x))

    elseif (trim(func) == "\sqrt" .or. trim(func) == "\SQRT") then

      fx = sqrt(x)

    elseif (trim(func) == "\abs" .or. trim(func) == "\ABS") then

      fx = abs(x)

    elseif (trim(func) == "\exp" .or. trim(func) == "\EXP") then

      fx = exp(x)

    elseif (trim(func) == "\ln" .or. trim(func) == "\LN") then

      fx = log(x)

    elseif (trim(func) == "\log" .or. trim(func) == "\LOG") then

      fx = log10(x)

    elseif (trim(func) == "\acos" .or. trim(func) == "\ACOS") then

      fx = acos(x)

    elseif (trim(func) == "\asin" .or. trim(func) == "\ASIN") then

      fx = asin(x)

    elseif (trim(func) == "\atan" .or. trim(func) == "\ATAN") then

      fx = atan(x)

    elseif (trim(func) == "\rand" .or. trim(func) == "\RAND") then

      call random_number(r)
      fx = r*x

    else

      fx = 0.0_real64

    end if

  end subroutine f_of_x_r2fp64

  subroutine f_of_x_r3fp64(functionhandler_obj,func,x,fx)
    !! Evaluates function for rank-3 array fp64 input and output
    class(FEQParse_FunctionHandler) :: functionhandler_obj
    character(*),intent(in)  :: func
    real(real64),intent(in)  :: x(:,:,:)
    real(real64),intent(out) :: fx(:,:,:)
    ! Local
    real(real64)   :: r
    integer :: i

    if (trim(func) == "\cos" .or. trim(func) == "\COS") then

      fx = cos(x)

    elseif (trim(func) == "\sin" .or. trim(func) == "\SIN") then

      fx = sin(x)

    elseif (trim(func) == "\tan" .or. trim(func) == "\TAN") then

      fx = tan(x)

    elseif (trim(func) == "\tanh" .or. trim(func) == "\TANH") then

      fx = tanh(x)

    elseif (trim(func) == "\sech" .or. trim(func) == "\SECH") then

      fx = 2.0_real64/(exp(x) + exp(-x))

    elseif (trim(func) == "\sqrt" .or. trim(func) == "\SQRT") then

      fx = sqrt(x)

    elseif (trim(func) == "\abs" .or. trim(func) == "\ABS") then

      fx = abs(x)

    elseif (trim(func) == "\exp" .or. trim(func) == "\EXP") then

      fx = exp(x)

    elseif (trim(func) == "\ln" .or. trim(func) == "\LN") then

      fx = log(x)

    elseif (trim(func) == "\log" .or. trim(func) == "\LOG") then

      fx = log10(x)

    elseif (trim(func) == "\acos" .or. trim(func) == "\ACOS") then

      fx = acos(x)

    elseif (trim(func) == "\asin" .or. trim(func) == "\ASIN") then

      fx = asin(x)

    elseif (trim(func) == "\atan" .or. trim(func) == "\ATAN") then

      fx = atan(x)

    elseif (trim(func) == "\rand" .or. trim(func) == "\RAND") then

      call random_number(r)
      fx = r*x

    else

      fx = 0.0_real64

    end if

  end subroutine f_of_x_r3fp64

  subroutine f_of_x_r4fp64(functionhandler_obj,func,x,fx)
    !! Evaluates function for rank-4 array fp64 input and output
    class(FEQParse_FunctionHandler) :: functionhandler_obj
    character(*),intent(in)  :: func
    real(real64),intent(in)  :: x(:,:,:,:)
    real(real64),intent(out) :: fx(:,:,:,:)
    ! Local
    real(real64)   :: r
    integer :: i

    if (trim(func) == "\cos" .or. trim(func) == "\COS") then

      fx = cos(x)

    elseif (trim(func) == "\sin" .or. trim(func) == "\SIN") then

      fx = sin(x)

    elseif (trim(func) == "\tan" .or. trim(func) == "\TAN") then

      fx = tan(x)

    elseif (trim(func) == "\tanh" .or. trim(func) == "\TANH") then

      fx = tanh(x)

    elseif (trim(func) == "\sech" .or. trim(func) == "\SECH") then

      fx = 2.0_real64/(exp(x) + exp(-x))

    elseif (trim(func) == "\sqrt" .or. trim(func) == "\SQRT") then

      fx = sqrt(x)

    elseif (trim(func) == "\abs" .or. trim(func) == "\ABS") then

      fx = abs(x)

    elseif (trim(func) == "\exp" .or. trim(func) == "\EXP") then

      fx = exp(x)

    elseif (trim(func) == "\ln" .or. trim(func) == "\LN") then

      fx = log(x)

    elseif (trim(func) == "\log" .or. trim(func) == "\LOG") then

      fx = log10(x)

    elseif (trim(func) == "\acos" .or. trim(func) == "\ACOS") then

      fx = acos(x)

    elseif (trim(func) == "\asin" .or. trim(func) == "\ASIN") then

      fx = asin(x)

    elseif (trim(func) == "\atan" .or. trim(func) == "\ATAN") then

      fx = atan(x)

    elseif (trim(func) == "\rand" .or. trim(func) == "\RAND") then

      call random_number(r)
      fx = r*x

    else

      fx = 0.0_real64

    end if

  end subroutine f_of_x_r4fp64

end module FEQParse_Functions

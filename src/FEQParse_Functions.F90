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

  integer,public :: nFunctions = 17
  integer,protected,public :: maxFunctionLength = 0
  integer,parameter :: maxFunctions = 100
  logical :: isInitialized = .false.

  enum,bind(c)
    enumerator :: cos_function = 1
    enumerator :: cosh_function = 2
    enumerator :: sin_function = 3
    enumerator :: sinh_function = 4
    enumerator :: tan_function = 5
    enumerator :: tanh_function = 6
    enumerator :: sqrt_function = 7
    enumerator :: abs_function = 8
    enumerator :: exp_function = 9
    enumerator :: ln_function = 10
    enumerator :: log_function = 11
    enumerator :: log10_function = 12
    enumerator :: acos_function = 13
    enumerator :: asin_function = 14
    enumerator :: atan_function = 15
    enumerator :: sech_function = 16
    enumerator :: rand_function = 17
  endenum

  private

  public :: InitializeFunctions, &
            AddFunction

  interface
    pure real(real32) function f32(x)
      import
      real(real32),intent(in) :: x
    endfunction
  endinterface

  interface
    pure real(real64) function f64(x)
      import
      real(real64),intent(in) :: x
    endfunction
  endinterface

  type Tuple
    character(:),allocatable :: item1
    character(:),allocatable :: item2
  contains
    final :: Tuple_Finalize
  endtype

  type,public :: FEQParse_Function
    integer :: len
    character(:),allocatable :: str
    character(:),allocatable :: caps
    procedure(f32),public,nopass,pointer :: ptr32 => null()
    procedure(f64),public,nopass,pointer :: ptr64 => null()
  contains
    procedure,private,pass(lhs) :: character_array_assign_function
    procedure,private,pass(lhs) :: character_assign_function
    generic :: assignment(=) => character_assign_function,character_array_assign_function
    procedure,private,pass(lhs) :: function_eq_character
    procedure,private,pass(rhs) :: character_eq_function
    generic :: operator(==) => function_eq_character,character_eq_function
    procedure,private,pass(lhs) :: function_neq_character
    procedure,private,pass(rhs) :: character_neq_function
    generic :: operator(/=) => function_neq_character,character_neq_function
    procedure,private,pass(this) :: invoke32
    procedure,private,pass(this) :: invoke64
    generic :: invoke => invoke32,invoke64
    final :: Function_Finalize
  endtype FEQParse_Function

  type(FEQParse_Function),public :: Functions(maxFunctions)

  interface
    pure real(real32) function randomize_r32()
      import
    endfunction
  endinterface

  interface
    pure real(real64) function randomize_r64()
      import
    endfunction
  endinterface

  interface AddFunction
    module procedure :: AddFunction32
    module procedure :: AddFunction64
  endinterface

  interface Tuple
    module procedure Tuple_new
  endinterface

contains

  type(Tuple) function Tuple_new(item1,item2) result(t)
    character(*),intent(in) :: item1
    character(*),intent(in) :: item2

    t%item1 = item1
    t%item2 = item2
  endfunction

  subroutine Tuple_Finalize(this)
    type(Tuple),intent(inout) :: this
    if(allocated(this%item1)) deallocate(this%item1)
    if(allocated(this%item2)) deallocate(this%item2)
  endsubroutine

  subroutine Function_Finalize(this)
    type(FEQParse_Function),intent(inout) :: this
    if(allocated(this%str)) deallocate(this%str)
    if(allocated(this%caps)) deallocate(this%caps)
    if(associated(this%ptr32)) nullify(this%ptr32)
    if(associated(this%ptr64)) nullify(this%ptr64)
  endsubroutine

  subroutine character_array_assign_function(lhs,rhs)
    class(FEQParse_Function),intent(inout) :: lhs !! Left hand side.
    class(Tuple),intent(in)    :: rhs !! Right hand side.

    lhs%str = rhs%item1
    lhs%len = len(rhs%item1)
    lhs%caps = rhs%item2
    maxFunctionLength = max(maxFunctionLength,lhs%len)
  endsubroutine

  subroutine character_assign_function(lhs,rhs)
    class(FEQParse_Function),intent(inout) :: lhs !! Left hand side.
    character(len=*),intent(in)    :: rhs !! Right hand side.

    lhs%str = rhs
    lhs%len = len(rhs)
    lhs%caps = ToUpperCase(rhs)
    maxFunctionLength = max(maxFunctionLength,lhs%len)
  endsubroutine

  elemental function function_eq_character(lhs,rhs) result(ok)
    class(FEQParse_Function),intent(in) :: lhs !! Left hand side.
    character(len=*),intent(in) :: rhs !! Right hand side.
    logical                   :: ok

    ok = lhs%str == rhs .or. lhs%caps == rhs
  endfunction

  elemental function character_eq_function(lhs,rhs) result(ok)
    character(len=*),intent(in) :: lhs
    class(FEQParse_Function),intent(in) :: rhs
    logical                   :: ok

    ok = lhs == rhs%str .or. lhs == rhs%caps
  endfunction

  elemental function function_neq_character(lhs,rhs) result(ok)
    class(FEQParse_Function),intent(in) :: lhs !! Left hand side.
    character(len=*),intent(in) :: rhs !! Right hand side.
    logical                   :: ok

    ok = lhs%str /= rhs .or. lhs%caps /= rhs
  endfunction

  elemental function character_neq_function(lhs,rhs) result(ok)
    character(len=*),intent(in) :: lhs
    class(FEQParse_Function),intent(in) :: rhs
    logical                   :: ok

    ok = lhs /= rhs%str .or. lhs /= rhs%caps
  endfunction

  subroutine InitializeFunctions()
    if(isInitialized .eqv. .true.) return

    Functions(cos_function) = Tuple("cos","COS")
    Functions(cos_function)%ptr32 => cos32
    Functions(cos_function)%ptr64 => cos64

    Functions(cosh_function) = Tuple("cosh","COSH")
    Functions(cosh_function)%ptr32 => cosh32
    Functions(cosh_function)%ptr64 => cosh64

    Functions(sin_function) = Tuple("sin","SIN")
    Functions(sin_function)%ptr32 => sin32
    Functions(sin_function)%ptr64 => sin64

    Functions(sinh_function) = Tuple("sinh","SINH")
    Functions(sinh_function)%ptr32 => sinh32
    Functions(sinh_function)%ptr64 => sinh64

    Functions(tan_function) = Tuple("tan","TAN")
    Functions(tan_function)%ptr32 => tan32
    Functions(tan_function)%ptr64 => tan64

    Functions(tanh_function) = Tuple("tanh","TANH")
    Functions(tanh_function)%ptr32 => tanh32
    Functions(tanh_function)%ptr64 => tanh64

    Functions(sqrt_function) = Tuple("sqrt","SQRT")
    Functions(sqrt_function)%ptr32 => sqrt32
    Functions(sqrt_function)%ptr64 => sqrt64

    Functions(abs_function) = Tuple("abs","ABS")
    Functions(abs_function)%ptr32 => abs32
    Functions(abs_function)%ptr64 => abs64

    Functions(exp_function) = Tuple("exp","EXP")
    Functions(exp_function)%ptr32 => exp32
    Functions(exp_function)%ptr64 => exp64

    Functions(ln_function) = Tuple("ln","LN")
    Functions(ln_function)%ptr32 => log32
    Functions(ln_function)%ptr64 => log64

    Functions(log_function) = Tuple("log","LOG")
    Functions(log_function)%ptr32 => log32
    Functions(log_function)%ptr64 => log64

    Functions(log10_function) = Tuple("log10","LOG10")
    Functions(log10_function)%ptr32 => log1032
    Functions(log10_function)%ptr64 => log1064

    Functions(acos_function) = Tuple("acos","ACOS")
    Functions(acos_function)%ptr32 => acos32
    Functions(acos_function)%ptr64 => acos64

    Functions(asin_function) = Tuple("asin","ASIN")
    Functions(asin_function)%ptr32 => asin32
    Functions(asin_function)%ptr64 => asin64

    Functions(atan_function) = Tuple("atan","ATAN")
    Functions(atan_function)%ptr32 => atan32
    Functions(atan_function)%ptr64 => atan64

    Functions(sech_function) = Tuple("sech","SECH")
    Functions(sech_function)%ptr32 => sech32
    Functions(sech_function)%ptr64 => sech64

    Functions(rand_function) = Tuple("rand","RAND")
    Functions(rand_function)%ptr32 => rand32
    Functions(rand_function)%ptr64 => rand64

    isInitialized = .true.
  endsubroutine InitializeFunctions

  subroutine AddFunction32(name,f_32)
    character(*),intent(in) :: name
    procedure(f32) :: f_32
    !private
    type(FEQParse_Function) :: func

    call InitializeFunctions()
    func = name
    func%ptr32 => f_32
    func%ptr64 => null()
    if(nFunctions < maxFunctions) then
      Functions(nFunctions+1) = func
      nFunctions = nFunctions+1
    else
      stop 'Argument out of range'
    endif
  endsubroutine

  subroutine AddFunction64(name,f_64)
    character(*),intent(in) :: name
    procedure(f64) :: f_64
    !private
    type(FEQParse_Function) :: func

    call InitializeFunctions()
    func = name
    func%ptr32 => null()
    func%ptr64 => f_64
    if(nFunctions < maxFunctions) then
      Functions(nFunctions+1) = func
      nFunctions = nFunctions+1
    else
      stop 'Argument out of range'
    endif
  endsubroutine

  elemental real(real32) function invoke32(this,x) result(fx)
    class(FEQParse_Function),intent(in) :: this
    real(real32),intent(in) :: x
    fx = this%ptr32(x)
  endfunction

  elemental real(real64) function invoke64(this,x) result(fx)
    class(FEQParse_Function),intent(in) :: this
    real(real64),intent(in) :: x
    fx = this%ptr64(x)
  endfunction

  pure real(real32) function cos32(x) result(fx)
    real(real32),intent(in) :: x
    fx = cos(x)
  endfunction

  pure real(real64) function cos64(x) result(fx)
    real(real64),intent(in) :: x
    fx = cos(x)
  endfunction

  pure real(real32) function cosh32(x) result(fx)
    real(real32),intent(in) :: x
    fx = cosh(x)
  endfunction

  pure real(real64) function cosh64(x) result(fx)
    real(real64),intent(in) :: x
    fx = cosh(x)
  endfunction

  pure real(real32) function sin32(x) result(fx)
    real(real32),intent(in) :: x
    fx = sin(x)
  endfunction

  pure real(real64) function sin64(x) result(fx)
    real(real64),intent(in) :: x
    fx = sin(x)
  endfunction

  pure real(real32) function sinh32(x) result(fx)
    real(real32),intent(in) :: x
    fx = sinh(x)
  endfunction

  pure real(real64) function sinh64(x) result(fx)
    real(real64),intent(in) :: x
    fx = sinh(x)
  endfunction

  pure real(real32) function tan32(x) result(fx)
    real(real32),intent(in) :: x
    fx = tan(x)
  endfunction

  pure real(real64) function tan64(x) result(fx)
    real(real64),intent(in) :: x
    fx = tan(x)
  endfunction

  pure real(real32) function tanh32(x) result(fx)
    real(real32),intent(in) :: x
    fx = tanh(x)
  endfunction

  pure real(real64) function tanh64(x) result(fx)
    real(real64),intent(in) :: x
    fx = tanh(x)
  endfunction

  pure real(real32) function sqrt32(x) result(fx)
    real(real32),intent(in) :: x
    fx = sqrt(x)
  endfunction

  pure real(real64) function sqrt64(x) result(fx)
    real(real64),intent(in) :: x
    fx = sqrt(x)
  endfunction

  pure real(real32) function abs32(x) result(fx)
    real(real32),intent(in) :: x
    fx = abs(x)
  endfunction

  pure real(real64) function abs64(x) result(fx)
    real(real64),intent(in) :: x
    fx = abs(x)
  endfunction

  pure real(real32) function exp32(x) result(fx)
    real(real32),intent(in) :: x
    fx = exp(x)
  endfunction

  pure real(real64) function exp64(x) result(fx)
    real(real64),intent(in) :: x
    fx = exp(x)
  endfunction

  pure real(real32) function log32(x) result(fx)
    real(real32),intent(in) :: x
    fx = log(x)
  endfunction

  pure real(real64) function log64(x) result(fx)
    real(real64),intent(in) :: x
    fx = log(x)
  endfunction

  pure real(real32) function log1032(x) result(fx)
    real(real32),intent(in) :: x
    fx = log10(x)
  endfunction

  pure real(real64) function log1064(x) result(fx)
    real(real64),intent(in) :: x
    fx = log10(x)
  endfunction

  pure real(real32) function acos32(x) result(fx)
    real(real32),intent(in) :: x
    fx = acos(x)
  endfunction

  pure real(real64) function acos64(x) result(fx)
    real(real64),intent(in) :: x
    fx = acos(x)
  endfunction

  pure real(real32) function asin32(x) result(fx)
    real(real32),intent(in) :: x
    fx = asin(x)
  endfunction

  pure real(real64) function asin64(x) result(fx)
    real(real64),intent(in) :: x
    fx = asin(x)
  endfunction

  pure real(real32) function atan32(x) result(fx)
    real(real32),intent(in) :: x
    fx = atan(x)
  endfunction

  pure real(real64) function atan64(x) result(fx)
    real(real64),intent(in) :: x
    fx = atan(x)
  endfunction

  pure real(real32) function sech32(x) result(fx)
    real(real32),intent(in) :: x
    fx = 2.0_real32/(exp(x)+exp(-x))
  endfunction

  pure real(real64) function sech64(x) result(fx)
    real(real64),intent(in) :: x
    fx = 2.0_real64/(exp(x)+exp(-x))
  endfunction

  pure real(real32) function rand32(x) result(fx)
    real(real32),intent(in) :: x
    !private
    real(real32) :: r
    associate(r => randomize_r32())
      fx = r*x
    endassociate
  endfunction

  pure real(real64) function rand64(x) result(fx)
    real(real64),intent(in) :: x
    !private
    real(real64) :: r
    associate(r => randomize_r64())
      fx = r*x
    endassociate
  endfunction

  pure function ToUpperCase(str) result(res)
    character(*),intent(in) :: str
    character(len(str)) :: res
    integer :: i

    do i = 1,len(str)
      select case(str(i:i))
      case('a':'z')
        res(i:i) = achar(iachar(str(i:i))-32)
      case default
        res(i:i) = str(i:i)
      endselect
    enddo
  endfunction ToUpperCase

endmodule FEQParse_Functions

real(real32) function randomize_r32()
  use,intrinsic :: iso_fortran_env,only:real32
  real(real32)   :: r

  call random_number(r)
  randomize_r32 = r
endfunction

real(real64) function randomize_r64()
  use,intrinsic :: iso_fortran_env,only:real64
  real(real64)   :: r

  call random_number(r)
  randomize_r64 = r
endfunction

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

    integer, protected, public :: nFunctions = 17
    integer, protected, public :: maxFunctionLength = 0

    private
    
    public :: InitializeFunctions, &
              f_of_x

    type, public :: FEQParse_Function
        integer :: len
        character(:), allocatable :: str
        character(:), allocatable :: caps
    contains
        procedure, private, pass(lhs) :: character_assign_function
        procedure, private, pass(rhs) :: function_assign_character
        generic :: assignment(=) => character_assign_function
        procedure, private, pass(lhs) :: function_eq_character
        procedure, private, pass(rhs) :: character_eq_function
        generic :: operator(==) => function_eq_character, character_eq_function
        procedure, private, pass(lhs) :: function_neq_character
        procedure, private, pass(rhs) :: character_neq_function
        generic :: operator(/=) => function_neq_character, character_neq_function
    end type FEQParse_Function

    type(FEQParse_Function), dimension(:), allocatable, public :: Functions

    interface f_of_x
        module procedure :: f_of_x_sfp32
        module procedure :: f_of_x_sfp64
    end interface

    interface
        pure real(real32) function randomize_r32()
            import
        end function
    end interface

    interface
        pure real(real64) function randomize_r64()
            import
        end function
    end interface

contains

    subroutine character_assign_function(lhs, rhs)
        class(FEQParse_Function), intent(inout) :: lhs !! Left hand side.
        character(len=*), intent(in)    :: rhs !! Right hand side.

        lhs%str = rhs
        lhs%len = len(rhs)
        lhs%caps = ToUpperCase(rhs)
        maxFunctionLength = max(maxFunctionLength, lhs%len)
    end subroutine

    pure subroutine function_assign_character(lhs, rhs)
        character(len=*), allocatable, intent(inout)    :: lhs
        class(FEQParse_Function), intent(in) :: rhs
        lhs = rhs%str
    end subroutine

    elemental function function_eq_character(lhs, rhs) result(ok)
        class(FEQParse_Function), intent(in) :: lhs !! Left hand side.
        character(len=*), intent(in) :: rhs !! Right hand side.
        logical                   :: ok

        ok = lhs%str == rhs .or. lhs%caps == rhs
    end function

    elemental function character_eq_function(lhs, rhs) result(ok)
        character(len=*), intent(in) :: lhs
        class(FEQParse_Function), intent(in) :: rhs
        logical                   :: ok

        ok = lhs == rhs%str .or. lhs == rhs%caps
    end function

    elemental function function_neq_character(lhs, rhs) result(ok)
        class(FEQParse_Function), intent(in) :: lhs !! Left hand side.
        character(len=*), intent(in) :: rhs !! Right hand side.
        logical                   :: ok

        ok = lhs%str /= rhs .or. lhs%caps /= rhs
    end function

    elemental function character_neq_function(lhs, rhs) result(ok)
        character(len=*), intent(in) :: lhs
        class(FEQParse_Function), intent(in) :: rhs
        logical                   :: ok

        ok = lhs /= rhs%str .or. lhs /= rhs%caps
    end function

    subroutine InitializeFunctions()
        if (allocated(Functions)) return
        
        allocate (Functions(1:nFunctions))
        Functions(1) = "cos"
        Functions(2) = "cosh"
        Functions(3) = "sin"
        Functions(4) = "sinh"
        Functions(5) = "tan"
        Functions(6) = "tanh"
        Functions(7) = "sqrt"
        Functions(8) = "abs"
        Functions(9) = "exp"
        Functions(10) = "ln"
        Functions(11) = "log"
        Functions(12) = "log10"
        Functions(13) = "acos"
        Functions(14) = "asin"
        Functions(15) = "atan"
        Functions(16) = "sech"
        Functions(17) = "rand"
    end subroutine InitializeFunctions

    elemental real(real32) function f_of_x_sfp32(func, x) result(fx)
    !! Evaluates function for scalar fp32 input and output
        character(*), intent(in) :: func
        real(real32), intent(in) :: x
        ! Local
        real(real32)   :: r

        if (func == "cos") then

            fx = cos(x)

        elseif (func == "cosh") then

            fx = cosh(x)

        elseif (func == "sin") then

            fx = sin(x)

        elseif (func == "sinh") then

            fx = sinh(x)

        elseif (func == "tan") then

            fx = tan(x)

        elseif (func == "tanh") then

            fx = tanh(x)

        elseif (func == "sech") then

            fx = 2.0_real32 / (exp(x) + exp(-x))

        elseif (func == "sqrt") then

            fx = sqrt(x)

        elseif (func == "abs") then

            fx = abs(x)

        elseif (func == "exp") then

            fx = exp(x)

        elseif (func == "ln") then

            fx = log(x)

        elseif (func == "log") then

            fx = log(x)

        elseif (func == "log10") then

            fx = log10(x)

        elseif (func == "acos") then

            fx = acos(x)

        elseif (func == "asin") then

            fx = asin(x)

        elseif (func == "atan") then

            fx = atan(x)

        elseif (func == "rand") then

            associate (r => randomize_r32())
                fx = r * x
            end associate
        else

            fx = 0.0_real32

        end if

    end function f_of_x_sfp32

    elemental real(real64) function f_of_x_sfp64(func, x) result(fx)
    !! Evaluates function for scalar fp64 input and output
        character(*), intent(in) :: func
        real(real64), intent(in)  :: x

        if (func == "cos") then

            fx = cos(x)

        elseif (func == "cosh") then

            fx = cosh(x)

        elseif (func == "sin") then

            fx = sin(x)

        elseif (func == "sinh") then

            fx = sinh(x)

        elseif (func == "tan") then

            fx = tan(x)

        elseif (func == "tanh") then

            fx = tanh(x)

        elseif (func == "sech") then

            fx = 2.0_real64 / (exp(x) + exp(-x))

        elseif (func == "sqrt") then

            fx = sqrt(x)

        elseif (func == "abs") then

            fx = abs(x)

        elseif (func == "exp") then

            fx = exp(x)

        elseif (func == "ln") then

            fx = log(x)

        elseif (func == "log") then

            fx = log(x)

        elseif (func == "log10") then

            fx = log10(x)

        elseif (func == "acos") then

            fx = acos(x)

        elseif (func == "asin") then

            fx = asin(x)

        elseif (func == "atan") then

            fx = atan(x)

        elseif (func == "rand") then

            associate (r => randomize_r64())
                fx = r * x
            end associate

        else

            fx = 0.0_real64

        end if

    end function f_of_x_sfp64

    pure function ToUpperCase(str) result(res)
        character(*), intent(in) :: str
        character(len(str)) :: res
        integer :: i, j

        do i = 1, len(str)
            select case (str(i:i))
            case ('a':'z')
                res(i:i) = achar(iachar(str(i:i)) - 32)
            case default
                res(i:i) = str(i:i)
            end select
        end do
    end function ToUpperCase

    pure function ToLowerCase(str) result(res)
        character(*), intent(in) :: str
        character(len(str)), allocatable :: res
        integer :: i

        do i = 1, len(str)
            select case (str(i:i))
            case ('A':'Z')
                res(i:i) = achar(iachar(str(i:i)) + 32)
            case default
                res(i:i) = str(i:i)
            end select
        end do
    end function ToLowerCase

end module FEQParse_Functions

real(real32) function randomize_r32()
    use, intrinsic :: iso_fortran_env, only: real32
    real(real32)   :: r

    call random_number(r)
    randomize_r32 = r
end function

real(real64) function randomize_r64()
    use, intrinsic :: iso_fortran_env, only: real64
    real(real64)   :: r

    call random_number(r)
    randomize_r64 = r
end function

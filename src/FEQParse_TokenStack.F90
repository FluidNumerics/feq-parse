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

module FEQParse_TokenStack
    use iso_fortran_env

    implicit none

    type Token
        character(48) :: tokenString
        integer       :: tokenType
        integer       :: tokenIndex
    contains
        procedure :: Copy
    end type

    type TokenStack
        type(Token), allocatable :: tokens(:)
        integer                  :: top_index = 0
    contains
        procedure :: Construct => Construct_TokenStack
        procedure :: Push => Push_TokenStack
        procedure :: Pop => Pop_TokenStack
        procedure :: IsEmpty => IsEmpty_TokenStack
        procedure :: TopToken
    end type

contains

    subroutine Construct_TokenStack(stack, N)
        class(TokenStack), intent(out) :: stack
        integer, intent(in)            :: N

        allocate (stack%tokens(1:N))
        stack%top_index = 0

    end subroutine Construct_TokenStack

    subroutine Push_TokenStack(stack, tok)
        class(TokenStack), intent(inout) :: stack
        type(Token), intent(in)         :: tok

        stack%top_index = stack%top_index + 1
        stack%tokens(stack%top_index)%tokenString = tok%tokenString
        stack%tokens(stack%top_index)%tokenType = tok%tokenType
        stack%tokens(stack%top_index)%tokenIndex = tok%tokenIndex
    end subroutine Push_TokenStack

    subroutine Pop_TokenStack(stack, tok)
        class(TokenStack), intent(inout) :: stack
        type(Token), intent(out)        :: tok

        if (stack%top_index <= 0) then
            print*,"Attempt to pop from empty token stack"
        else
            tok%tokenString = stack%tokens(stack%top_index)%tokenString
            tok%tokenType = stack%tokens(stack%top_index)%tokenType
            tok%tokenIndex = stack%tokens(stack%top_index)%tokenIndex
            stack%top_index = stack%top_index - 1
        end if

    end subroutine Pop_TokenStack

    logical function IsEmpty_TokenStack(stack)
        class(TokenStack) :: stack

        IsEmpty_TokenStack = .false.

        if (stack%top_index <= 0) then
            IsEmpty_TokenStack = .true.
        end if

    end function IsEmpty_TokenStack

    type(Token) function TopToken(stack)
        class(TokenStack) :: stack

        if (stack%top_index > 0) then
            TopToken%tokenString = stack%tokens(stack%top_index)%tokenString
            TopToken%tokenType = stack%tokens(stack%top_index)%tokenType
            TopToken%tokenIndex = stack%tokens(stack%top_index)%tokenIndex
        else
            TopToken%tokenString = ''
        end if

    end function TopToken

    function Copy(this) result(that)
        class(Token) :: this
        type(Token)  :: that

        that%tokenString = this%tokenString
        that%tokenType = this%tokenType
        that%tokenIndex = this%tokenIndex
    end function Copy

end module FEQParse_TokenStack

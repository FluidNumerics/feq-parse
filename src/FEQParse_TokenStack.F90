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

  integer,parameter :: Token_Length = 48

  type Token
    character(Token_Length) :: tokenString
    integer                 :: tokenType

  contains
    procedure :: Equals_Token

  end type Token

  type TokenStack
    type(Token),allocatable :: tokens(:)
    integer                  :: top_index = 0

  contains

    procedure :: Construct => Construct_TokenStack
    procedure :: Destruct => Destruct_TokenStack

    procedure :: Push => Push_TokenStack
    procedure :: Pop => Pop_TokenStack

    procedure :: IsEmpty => IsEmpty_TokenStack
    procedure :: TopToken

  end type TokenStack

contains

  subroutine Construct_TokenStack(stack,N)
    class(TokenStack),intent(out) :: stack
    integer,intent(in)            :: N

    allocate (stack % tokens(1:N))
    stack % top_index = 0

  end subroutine Construct_TokenStack

  subroutine Destruct_TokenStack(stack)
    class(TokenStack),intent(inout) :: stack

    if (allocated(stack % tokens)) deallocate (stack % tokens)
    stack % top_index = 0

  end subroutine Destruct_TokenStack

  subroutine Push_TokenStack(stack,tok)
    class(TokenStack),intent(inout) :: stack
    type(Token),intent(in)         :: tok

    stack % top_index = stack % top_index + 1
    stack % tokens(stack % top_index) % tokenString = tok % tokenString
    stack % tokens(stack % top_index) % tokenType = tok % tokenType

  end subroutine Push_TokenStack

  subroutine Pop_TokenStack(stack,tok)
    class(TokenStack),intent(inout) :: stack
    type(Token),intent(out)        :: tok

    if (stack % top_index <= 0) then
      print *, "Attempt to pop from empty token stack"
    else
      tok % tokenString = stack % tokens(stack % top_index) % tokenString
      tok % tokenType = stack % tokens(stack % top_index) % tokenType
      stack % top_index = stack % top_index - 1
    end if

  end subroutine Pop_TokenStack

  logical function IsEmpty_TokenStack(stack)
    class(TokenStack) :: stack

    IsEmpty_TokenStack = .false.

    if (stack % top_index <= 0) then
      IsEmpty_TokenStack = .true.
    end if

  end function IsEmpty_TokenStack

  type(Token) function TopToken(stack)
    class(TokenStack) :: stack

    if (stack % top_index > 0) then
      TopToken % tokenString = stack % tokens(stack % top_index) % tokenString
      TopToken % tokenType = stack % tokens(stack % top_index) % tokenType
    else
      TopToken % tokenString = ''
    end if

  end function TopToken

  function Equals_Token(tok1) result(tok2)
    class(Token) :: tok1
    type(Token)  :: tok2

    tok2 % tokenString = tok1 % tokenString
    tok2 % tokenType = tok1 % tokenType

  end function Equals_Token

end module FEQParse_TokenStack

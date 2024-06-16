! //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////// !
!
! Maintainers : support@fluidnumerics.com
! Official Repository : https://github.com/FluidNumerics/feq-parse/
!
! Copyright © 2024 Fluid Numerics LLC
!
! Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:
!
! 1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
!
! 2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in 
!    the documentation and/or other materials provided with the distribution.
!
! 3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived from 
!    this software without specific prior written permission.
!
! THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS “AS IS” AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
! LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
! HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT 
! LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY 
! THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF 
! THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
!
! //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////// !

module FEQParse_TokenStack
  use iso_fortran_env

  implicit none

  type Token
    character(48) :: tokenString
    integer       :: tokenType
    integer       :: tokenIndex
  contains
    procedure :: Copy
  endtype

  type TokenStack
    type(Token),allocatable :: tokens(:)
    integer                  :: top_index = 0
  contains
    procedure :: Construct => Construct_TokenStack
    procedure :: Finalize => Finalize_TokenStack
    procedure :: Push => Push_TokenStack
    procedure :: Pop => Pop_TokenStack
    procedure :: IsEmpty => IsEmpty_TokenStack
    procedure :: TopToken
  endtype

contains

  subroutine Construct_TokenStack(stack,N)
    class(TokenStack),intent(out) :: stack
    integer,intent(in)            :: N

    allocate(stack%tokens(1:N))
    stack%top_index = 0

  endsubroutine Construct_TokenStack

  subroutine Finalize_TokenStack(stack)
    class(TokenStack),intent(inout) :: stack

    if(allocated(stack%tokens)) deallocate(stack%tokens)

  endsubroutine Finalize_TokenStack

  subroutine Push_TokenStack(stack,tok)
    class(TokenStack),intent(inout) :: stack
    type(Token),intent(in)         :: tok

    stack%top_index = stack%top_index+1
    stack%tokens(stack%top_index)%tokenString = tok%tokenString
    stack%tokens(stack%top_index)%tokenType = tok%tokenType
    stack%tokens(stack%top_index)%tokenIndex = tok%tokenIndex
  endsubroutine Push_TokenStack

  subroutine Pop_TokenStack(stack,tok)
    class(TokenStack),intent(inout) :: stack
    type(Token),intent(out)        :: tok

    if(stack%top_index <= 0) then
      print*,"Attempt to pop from empty token stack"
    else
      tok%tokenString = stack%tokens(stack%top_index)%tokenString
      tok%tokenType = stack%tokens(stack%top_index)%tokenType
      tok%tokenIndex = stack%tokens(stack%top_index)%tokenIndex
      stack%top_index = stack%top_index-1
    endif

  endsubroutine Pop_TokenStack

  logical function IsEmpty_TokenStack(stack)
    class(TokenStack) :: stack

    IsEmpty_TokenStack = .false.

    if(stack%top_index <= 0) then
      IsEmpty_TokenStack = .true.
    endif

  endfunction IsEmpty_TokenStack

  type(Token) function TopToken(stack)
    class(TokenStack) :: stack

    if(stack%top_index > 0) then
      TopToken%tokenString = stack%tokens(stack%top_index)%tokenString
      TopToken%tokenType = stack%tokens(stack%top_index)%tokenType
      TopToken%tokenIndex = stack%tokens(stack%top_index)%tokenIndex
    else
      TopToken%tokenString = ''
    endif

  endfunction TopToken

  function Copy(this) result(that)
    class(Token) :: this
    type(Token)  :: that

    that%tokenString = this%tokenString
    that%tokenType = this%tokenType
    that%tokenIndex = this%tokenIndex
  endfunction Copy

endmodule FEQParse_TokenStack

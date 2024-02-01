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

module FEQParse_FloatStacks

    use iso_fortran_env

    implicit none

    type feqparse_floatstack
        integer :: top_index
    end type feqparse_floatstack

    type, extends(feqparse_floatstack) :: sfp32Stack
        real(real32), allocatable :: tokens(:)

    contains

        procedure :: Construct => Construct_sfp32Stack
        procedure :: Push => Push_sfp32Stack
        procedure :: Pop => Pop_sfp32Stack

    end type sfp32Stack

    type, extends(feqparse_floatstack) :: sfp64Stack
        real(real64), allocatable :: tokens(:)

    contains

        procedure :: Construct => Construct_sfp64Stack
        procedure :: Push => Push_sfp64Stack
        procedure :: Pop => Pop_sfp64Stack

    end type sfp64Stack

    type, extends(feqparse_floatstack) :: r1fp32Stack
        real(real32), allocatable :: tokens(:, :)

    contains

        procedure :: Construct => Construct_r1fp32Stack
        procedure :: Push => Push_r1fp32Stack
        procedure :: Pop => Pop_r1fp32Stack

    end type r1fp32Stack

    type, extends(feqparse_floatstack) :: r1fp64Stack
        real(real64), allocatable :: tokens(:, :)

    contains

        procedure :: Construct => Construct_r1fp64Stack
        procedure :: Push => Push_r1fp64Stack
        procedure :: Pop => Pop_r1fp64Stack

    end type r1fp64Stack

    type, extends(feqparse_floatstack) :: r2fp32Stack
        real(real32), allocatable :: tokens(:, :, :)

    contains

        procedure :: Construct => Construct_r2fp32Stack
        procedure :: Push => Push_r2fp32Stack
        procedure :: Pop => Pop_r2fp32Stack

    end type r2fp32Stack

    type, extends(feqparse_floatstack) :: r2fp64Stack
        real(real64), allocatable :: tokens(:, :, :)

    contains

        procedure :: Construct => Construct_r2fp64Stack
        procedure :: Push => Push_r2fp64Stack
        procedure :: Pop => Pop_r2fp64Stack

    end type r2fp64Stack

    type, extends(feqparse_floatstack) :: r3fp32Stack
        real(real32), allocatable :: tokens(:, :, :, :)

    contains

        procedure :: Construct => Construct_r3fp32Stack
        procedure :: Push => Push_r3fp32Stack
        procedure :: Pop => Pop_r3fp32Stack

    end type r3fp32Stack

    type, extends(feqparse_floatstack) :: r3fp64Stack
        real(real64), allocatable :: tokens(:, :, :, :)

    contains

        procedure :: Construct => Construct_r3fp64Stack
        procedure :: Push => Push_r3fp64Stack
        procedure :: Pop => Pop_r3fp64Stack

    end type r3fp64Stack

    type, extends(feqparse_floatstack) :: r4fp32Stack
        real(real32), allocatable :: tokens(:, :, :, :, :)

    contains

        procedure :: Construct => Construct_r4fp32Stack
        procedure :: Push => Push_r4fp32Stack
        procedure :: Pop => Pop_r4fp32Stack

    end type r4fp32Stack

    type, extends(feqparse_floatstack) :: r4fp64Stack
        real(real64), allocatable :: tokens(:, :, :, :, :)

    contains

        procedure :: Construct => Construct_r4fp64Stack
        procedure :: Push => Push_r4fp64Stack
        procedure :: Pop => Pop_r4fp64Stack

    end type r4fp64Stack

contains

    subroutine Construct_sfp32Stack(stack, N)
        class(sfp32Stack), intent(out) :: stack
        integer, intent(in)            :: N

        allocate (stack%tokens(1:N))
        stack%top_index = 0

    end subroutine Construct_sfp32Stack

    subroutine Push_sfp32Stack(stack, tok)
        class(sfp32Stack), intent(inout) :: stack
        real(real32), intent(in)         :: tok

        stack%top_index = stack%top_index + 1
        stack%tokens(stack%top_index) = tok

    end subroutine Push_sfp32Stack

    subroutine Pop_sfp32Stack(stack, tok)
        class(sfp32Stack), intent(inout) :: stack
        real(real32), intent(out)        :: tok

        if (stack%top_index <= 0) then
            print*,"Attempt to pop from empty token stack"
        else
            tok = stack%tokens(stack%top_index)
            stack%top_index = stack%top_index - 1
        end if

    end subroutine Pop_sfp32Stack

    subroutine Construct_sfp64Stack(stack, N)
        class(sfp64Stack), intent(out) :: stack
        integer, intent(in)            :: N

        allocate (stack%tokens(1:N))
        stack%top_index = 0

    end subroutine Construct_sfp64Stack

    subroutine Push_sfp64Stack(stack, tok)
        class(sfp64Stack), intent(inout) :: stack
        real(real64), intent(in)         :: tok

        stack%top_index = stack%top_index + 1
        stack%tokens(stack%top_index) = tok

    end subroutine Push_sfp64Stack

    subroutine Pop_sfp64Stack(stack, tok)
        class(sfp64Stack), intent(inout) :: stack
        real(real64), intent(out)        :: tok

        if (stack%top_index <= 0) then
            print*,"Attempt to pop from empty token stack"
        else
            tok = stack%tokens(stack%top_index)
            stack%top_index = stack%top_index - 1
        end if

    end subroutine Pop_sfp64Stack

    ! >> Rank 1 fp32, fp64 << !

    subroutine Construct_r1fp32Stack(stack, N, mold)
        implicit none
        class(r1fp32Stack), intent(out) :: stack
        integer, intent(in)             :: N
        real(real32), intent(in)       :: mold(:)
        ! local
        integer :: l(1)
        integer :: u(1)

        l = lbound(mold)
        u = ubound(mold)

        allocate (stack%tokens(l(1):u(1), 1:N))
        stack%top_index = 0

    end subroutine Construct_r1fp32Stack

    subroutine Push_r1fp32Stack(stack, tok)
        class(r1fp32Stack), intent(inout) :: stack
        real(real32), intent(in)          :: tok(:)

        stack%top_index = stack%top_index + 1
        stack%tokens(:, stack%top_index) = tok(:)

    end subroutine Push_r1fp32Stack

    subroutine Pop_r1fp32Stack(stack, tok)
        class(r1fp32Stack), intent(inout) :: stack
        real(real32), intent(out)        :: tok(:)

        if (stack%top_index <= 0) then
            print*,"Attempt to pop from empty token stack"
        else
            tok(:) = stack%tokens(:, stack%top_index)
            stack%top_index = stack%top_index - 1
        end if

    end subroutine Pop_r1fp32Stack

    subroutine Construct_r1fp64Stack(stack, N, mold)
        class(r1fp64Stack), intent(out) :: stack
        integer, intent(in)            :: N
        real(real64), intent(in)      :: mold(:)
        ! local
        integer :: l(1)
        integer :: u(1)

        l = lbound(mold)
        u = ubound(mold)

        allocate (stack%tokens(l(1):u(1), 1:N))
        stack%top_index = 0

    end subroutine Construct_r1fp64Stack

    subroutine Push_r1fp64Stack(stack, tok)
        class(r1fp64Stack), intent(inout) :: stack
        real(real64), intent(in)          :: tok(:)

        stack%top_index = stack%top_index + 1
        stack%tokens(:, stack%top_index) = tok(:)

    end subroutine Push_r1fp64Stack

    subroutine Pop_r1fp64Stack(stack, tok)
        class(r1fp64Stack), intent(inout) :: stack
        real(real64), intent(out)        :: tok(:)

        if (stack%top_index <= 0) then
            print*,"Attempt to pop from empty token stack"
        else
            tok(:) = stack%tokens(:, stack%top_index)
            stack%top_index = stack%top_index - 1
        end if

    end subroutine Pop_r1fp64Stack

    subroutine Construct_r2fp32Stack(stack, N, mold)
        class(r2fp32Stack), intent(out) :: stack
        integer, intent(in)             :: N
        real(real32), intent(in)       :: mold(:, :)
        ! local
        integer :: l(1:2), u(1:2)

        l = lbound(mold)
        u = ubound(mold)

        allocate (stack%tokens(l(1):u(1), &
                               l(2):u(2), &
                               1:N))
        stack%top_index = 0

    end subroutine Construct_r2fp32Stack

    subroutine Push_r2fp32Stack(stack, tok)
        class(r2fp32Stack), intent(inout) :: stack
        real(real32), intent(in)          :: tok(:, :)

        stack%top_index = stack%top_index + 1
        stack%tokens(:, :, stack%top_index) = tok(:, :)

    end subroutine Push_r2fp32Stack

    subroutine Pop_r2fp32Stack(stack, tok)
        class(r2fp32Stack), intent(inout) :: stack
        real(real32), intent(out)        :: tok(:, :)

        if (stack%top_index <= 0) then
            print*,"Attempt to pop from empty token stack"
        else
            tok(:, :) = stack%tokens(:, :, stack%top_index)
            stack%top_index = stack%top_index - 1
        end if

    end subroutine Pop_r2fp32Stack

    subroutine Construct_r2fp64Stack(stack, N, mold)
        class(r2fp64Stack), intent(out) :: stack
        integer, intent(in)            :: N
        real(real64), intent(in)      :: mold(:, :)
        ! local
        integer :: l(1:2), u(1:2)

        l = lbound(mold)
        u = ubound(mold)

        allocate (stack%tokens(l(1):u(1), &
                               l(2):u(2), &
                               1:N))
        stack%top_index = 0

    end subroutine Construct_r2fp64Stack

    subroutine Push_r2fp64Stack(stack, tok)
        class(r2fp64Stack), intent(inout) :: stack
        real(real64), intent(in)          :: tok(:, :)

        stack%top_index = stack%top_index + 1
        stack%tokens(:, :, stack%top_index) = tok(:, :)

    end subroutine Push_r2fp64Stack

    subroutine Pop_r2fp64Stack(stack, tok)
        class(r2fp64Stack), intent(inout) :: stack
        real(real64), intent(out)        :: tok(:, :)

        if (stack%top_index <= 0) then
            print*,"Attempt to pop from empty token stack"
        else
            tok(:, :) = stack%tokens(:, :, stack%top_index)
            stack%top_index = stack%top_index - 1
        end if

    end subroutine Pop_r2fp64Stack

    subroutine Construct_r3fp32Stack(stack, N, mold)
        class(r3fp32Stack), intent(out) :: stack
        integer, intent(in)             :: N
        real(real32), intent(in)       :: mold(:, :, :)
        ! local
        integer :: l(1:3), u(1:3)

        l = lbound(mold)
        u = ubound(mold)

        allocate (stack%tokens(l(1):u(1), &
                               l(2):u(2), &
                               l(3):u(3), &
                               1:N))
        stack%top_index = 0

    end subroutine Construct_r3fp32Stack

    subroutine Push_r3fp32Stack(stack, tok)
        class(r3fp32Stack), intent(inout) :: stack
        real(real32), intent(in)          :: tok(:, :, :)

        stack%top_index = stack%top_index + 1
        stack%tokens(:, :, :, stack%top_index) = tok(:, :, :)

    end subroutine Push_r3fp32Stack

    subroutine Pop_r3fp32Stack(stack, tok)
        class(r3fp32Stack), intent(inout) :: stack
        real(real32), intent(out)        :: tok(:, :, :)

        if (stack%top_index <= 0) then
            print*,"Attempt to pop from empty token stack"
        else
            tok(:, :, :) = stack%tokens(:, :, :, stack%top_index)
            stack%top_index = stack%top_index - 1
        end if

    end subroutine Pop_r3fp32Stack

    subroutine Construct_r3fp64Stack(stack, N, mold)
        class(r3fp64Stack), intent(out) :: stack
        integer, intent(in)            :: N
        real(real64), intent(in)      :: mold(:, :, :)
        ! local
        integer :: l(1:3), u(1:3)

        l = lbound(mold)
        u = ubound(mold)

        allocate (stack%tokens(l(1):u(1), &
                               l(2):u(2), &
                               l(3):u(3), &
                               1:N))
        stack%top_index = 0

    end subroutine Construct_r3fp64Stack

    subroutine Push_r3fp64Stack(stack, tok)
        class(r3fp64Stack), intent(inout) :: stack
        real(real64), intent(in)          :: tok(:, :, :)

        stack%top_index = stack%top_index + 1
        stack%tokens(:, :, :, stack%top_index) = tok(:, :, :)

    end subroutine Push_r3fp64Stack

    subroutine Pop_r3fp64Stack(stack, tok)
        class(r3fp64Stack), intent(inout) :: stack
        real(real64), intent(out)        :: tok(:, :, :)

        if (stack%top_index <= 0) then
            print*,"Attempt to pop from empty token stack"
        else
            tok(:, :, :) = stack%tokens(:, :, :, stack%top_index)
            stack%top_index = stack%top_index - 1
        end if

    end subroutine Pop_r3fp64Stack

    subroutine Construct_r4fp32Stack(stack, N, mold)
        class(r4fp32Stack), intent(out) :: stack
        integer, intent(in)             :: N
        real(real32), intent(in)       :: mold(:, :, :, :)
        ! local
        integer :: l(1:4), u(1:4)

        l = lbound(mold)
        u = ubound(mold)

        allocate (stack%tokens(l(1):u(1), &
                               l(2):u(2), &
                               l(3):u(3), &
                               l(4):u(4), &
                               1:N))
        stack%top_index = 0

    end subroutine Construct_r4fp32Stack

    subroutine Push_r4fp32Stack(stack, tok)
        class(r4fp32Stack), intent(inout) :: stack
        real(real32), intent(in)          :: tok(:, :, :, :)

        stack%top_index = stack%top_index + 1
        stack%tokens(:, :, :, :, stack%top_index) = tok(:, :, :, :)

    end subroutine Push_r4fp32Stack

    subroutine Pop_r4fp32Stack(stack, tok)
        class(r4fp32Stack), intent(inout) :: stack
        real(real32), intent(out)        :: tok(:, :, :, :)

        if (stack%top_index <= 0) then
            print*,"Attempt to pop from empty token stack"
        else
            tok(:, :, :, :) = stack%tokens(:, :, :, :, stack%top_index)
            stack%top_index = stack%top_index - 1
        end if

    end subroutine Pop_r4fp32Stack

    subroutine Construct_r4fp64Stack(stack, N, mold)
        class(r4fp64Stack), intent(out) :: stack
        integer, intent(in)            :: N
        real(real64), intent(in)      :: mold(:, :, :, :)
        ! local
        integer :: l(1:4), u(1:4)

        l = lbound(mold)
        u = ubound(mold)

        allocate (stack%tokens(l(1):u(1), &
                               l(2):u(2), &
                               l(3):u(3), &
                               l(4):u(4), &
                               1:N))
        stack%top_index = 0

    end subroutine Construct_r4fp64Stack

    subroutine Push_r4fp64Stack(stack, tok)
        class(r4fp64Stack), intent(inout) :: stack
        real(real64), intent(in)          :: tok(:, :, :, :)

        stack%top_index = stack%top_index + 1
        stack%tokens(:, :, :, :, stack%top_index) = tok(:, :, :, :)

    end subroutine Push_r4fp64Stack

    subroutine Pop_r4fp64Stack(stack, tok)
        class(r4fp64Stack), intent(inout) :: stack
        real(real64), intent(out)        :: tok(:, :, :, :)

        if (stack%top_index <= 0) then
            print*,"Attempt to pop from empty token stack"
        else
            tok(:, :, :, :) = stack%tokens(:, :, :, :, stack%top_index)
            stack%top_index = stack%top_index - 1
        end if

    end subroutine Pop_r4fp64Stack

end module FEQParse_FloatStacks

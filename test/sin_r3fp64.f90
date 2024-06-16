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

program test

  implicit none
  integer :: exit_code

  exit_code = sin_r3fp64()
  stop exit_code

contains

  integer function sin_r3fp64() result(r)
    use FEQParse
    use iso_fortran_env
    implicit none
    integer,parameter :: N = 10
    real(real64),parameter :: pi = 4.0_real64*atan(1.0_real64)
    type(EquationParser) :: f
    character(LEN=1),dimension(1:3) :: independentVars
    character(LEN=1024) :: eqChar
    real(real64),allocatable :: x(:,:,:,:)
    real(real64),allocatable :: feval(:,:,:)
    real(real64),allocatable :: fexact(:,:,:)
    integer :: i,j,k

    allocate(x(1:N,1:N,1:N,1:3), &
             feval(1:N,1:N,1:N), &
             fexact(1:N,1:N,1:N))

    ! Specify the independent variables
    independentVars = (/'x','y','z'/)

    ! Specify an equation string that we want to evaluate
    eqChar = 'f = sin( 2.0*pi*x )*sin( 2.0*pi*y )*sin(2.0*pi*z)'

    ! Create the EquationParser object
    f = EquationParser(eqChar,independentVars)

    x = 0.0_real64
    do k = 1,N
      do j = 1,N
        do i = 1,N
          x(i,j,k,1) = -1.0_real64+(2.0_real64)/real(N,real64)*real(i-1,real64)
          x(i,j,k,2) = -1.0_real64+(2.0_real64)/real(N,real64)*real(j-1,real64)
          x(i,j,k,3) = -1.0_real64+(2.0_real64)/real(N,real64)*real(k-1,real64)
        enddo
      enddo
    enddo
    do k = 1,N
      do j = 1,N
        do i = 1,N
          fexact(i,j,k) = sin(2.0_real64*pi*x(i,j,k,1))*sin(2.0_real64*pi*x(i,j,k,2))*sin(2.0_real64*pi*x(i,j,k,3))
        enddo
      enddo
    enddo

    ! Evaluate the equation
    feval = f%evaluate(x)
    if(maxval(abs(feval-fexact)) <= epsilon(1.0_real64)) then
      r = 0
    else
      r = 1
    endif

    deallocate(x,feval,fexact)

  endfunction sin_r3fp64
endprogram test

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

  exit_code = vanderpol_sfp64()
  stop exit_code

contains

  integer function vanderpol_sfp64() result(r)
    use FEQParse
    use iso_fortran_env
    implicit none
    integer,parameter :: N = 10
    type(EquationParser) :: f
    character(len=128) :: fun_buf
    real(real64) :: t,x,y,u,v
    real(real64) :: feval
    real(real64) :: fexact

    fun_buf = '-8.53*(1-x*x)*v-y'
    f = equationparser('f = '//fun_buf,['t','x','y','u','v'])

    print*,"--------- Infix -------------------"
    call f%Print_InFixTokens()
    print*,"-----------------------------------"
    print*,"--------- Postfix -------------------"
    call f%Print_PostFixTokens()
    print*,"-----------------------------------"

    t = 0.0_real64
    x = 1.0_real64
    y = 3.0_real64
    u = 12.0_real64
    v = 0.0_real64

    feval = f%evaluate([t,x,y,u,v])

    fexact = -8.53_real64*(1.0_real64-x*x)*v-y

    if((abs(feval-fexact)) <= epsilon(1.0_real64)) then
      r = 0
      print*,feval,fexact
    else
      r = 1
      print*,feval,fexact
    endif

  endfunction vanderpol_sfp64
endprogram test

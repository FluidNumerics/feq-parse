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

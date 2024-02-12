program test
  use iso_fortran_env,only:i1 => int8,i2 => int16,i4 => int32,i8 => int64, &
                            r4 => real32,r8 => real64,r16 => real128
  use FEQParse

  implicit none

  write(*,'(A,I20)') "test ",testing()

contains

  integer function testing() result(r)
    !private
    type(EquationParser) :: f
    real(r8) :: feval

    call AddFunction("myfunc",myfunc64)

    f = EquationParser("MYFUNC(x)",["x"])

    feval = f%evaluate([1.0_r8])
    if((abs(feval-0.5_r8)) <= epsilon(1.0_r8)) then
      r = 0
    else
      r = 1
    endif
  endfunction

  pure real(r8) function myfunc64(x)
    real(r8),intent(in) :: x
    myfunc64 = x/2.0_r8
  endfunction
endprogram

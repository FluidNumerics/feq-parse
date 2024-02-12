program test
  use iso_fortran_env,only:i1 => int8,i2 => int16,i4 => int32,i8 => int64, &
                            r4 => real32,r8 => real64,r16 => real128
  use FEQParse

  implicit none

  real(r8) :: results(24)
  integer j
  data(results(j),j=1,24)/0.9508_r8, &
    0.7958_r8, &
    0.2846_r8, &
    1.1247_r8, &
    0.9177_r8, &
    1.0599_r8, &
    0.8064_r8, &
    3.2193_r8, &
    0.9981_r8, &
    0.8366_r8, &
    0.9441_r8, &
    0.1457_r8, &
    4.2508_r8, &
    1.2064_r8, &
    1.2227_r8, &
    20.6962_r8, &
    112.1323_r8, &
    1.5597_r8, &
    7.5806_r8, &
    1.5574_r8, &
    0.8869_r8, &
    3.0118_r8, &
    5.4819_r8, &
    1.5311_r8/

  character(200) :: test_data(1:24)
  data(test_data(j),j=1,24)/ &
              'a+b*x1', &
      '(a*x**b)/(c+x**b)', &
      '(a*x)/(b+(x*(1+x/c)))', &
      'a+b*exp(c*x)+d*exp(e*x)', &
      'a+b*(exp(c*x) - 1)/c', &
      'a+b*log(x)+c*log(x)**2', &
      'a-log(1+b*exp(-c*x))', &
      '(a+b*x)/(c+x)', &
      'a+b*exp(-(c*x))', &
      'a+b*sin(2*3.14*x/c+d)', &
      'a+b*sin(2*4.14*x/c+d)**2', &
      '1-exp(-a*x)', &
      'a+b*x1+c*x2', &
      'a+b*log(x1)+c*log(x2)', &
      'a*x1**b*x2**c', &
      'cosh(log(abs(y*z+x**2+x1**x2)))+a*d*(exp(c*f)+154.3)', &
      'a+b*log(x1)+c*x2+d*x2**2', &
      'atan(sinh(log(abs(exp(z/x)*sqrt(y+a**c+f*e)))))', &
      'a+b/x1+c*log(x2)+d*log(x2)**2+e*log(x2)**3', &
      'atan(sinh(log(abs(exp(z/x)*sqrt(y+a**c+f*e)))))*cos(log(abs(sqrt(y+a**c+f*e))))', &
      'a+b*log(x1)+c*log(x1)**2+d/x2+e/x2**2', &
      '(x+a)/(b+c*(x+a)+d*(x+a)**2)', &
      '(x+y+z+x*y+x*z+y*z+x/y+x/z+y/z+x*cos(x)+y*sin(y)+z*tan(z)*2/(x+y+z+x*y+x*z+y*z+x/y+ &
      & x/z+y/z+x*cos(x)+y*sin(y)+z*tan(z))*3+sqrt(x*y*z+x+y+z)*log10(sqrt(x*2+y*2+z*2)+x+y+z))', &
              'a+b*log(x1)+c*log(x1)**2+d*log(x1)**3+e/x'/

  character(len=10),dimension(1:11) :: independentvars
  real(r8) :: x(1:11)

  call initialize()

  do j = 1,24
    write(*,'(A,A,I20)') "parsing ",test_data(j),parsing(j)
  enddo

contains

  subroutine initialize()
    independentvars = ['x ','y ','z ','x1','x2', &
                       'a ','b ','c ','d ','e ','f ']
    x(1) = 0.175_r8
    x(2) = 0.110_r8
    x(3) = 0.900_r8
    x(4) = 0.508_r8
    x(5) = 30.000_r8
    x(6) = 0.900_r8
    x(7) = 0.100_r8
    x(8) = 0.110_r8
    x(9) = 0.120_r8
    x(10) = 0.120_r8
    x(11) = 0.140_r8
  endsubroutine

  integer function parsing(i) result(r)
    integer,intent(in) :: i
    !private
    type(EquationParser) :: f
    real(r8) :: feval
    f = EquationParser(test_data(i),independentvars)
    feval = f%evaluate(x)
    if((abs(feval-results(i))) <= 0.001_r8) then
      r = 0
    else
      r = 1
    endif
  endfunction

endprogram

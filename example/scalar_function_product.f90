program scalar_function_product
    use iso_fortran_env
    use FEQParse

    implicit none
    type(EquationParser) :: f
    character(LEN=1), dimension(2) :: independentVars
    character(LEN=2048) :: eqChar
    real :: x(2)
    real :: feval

    ! Specify the independent variables
    independentVars = (/'x', 'y'/)

    ! Specify an equation string that we want to evaluate
    eqChar = 'f = cos( 2.0*pi*x )*cos( 2.0*pi*y )'

    ! Create the EquationParser object
    f = EquationParser(eqChar, independentVars)

    ! Evaluate the equation
    x = 0.5
    feval = f%evaluate(x)
    print*,feval

end program scalar_function_product

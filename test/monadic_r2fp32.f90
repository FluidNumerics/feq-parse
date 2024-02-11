program test

    implicit none
    integer :: exit_code

    exit_code = monadic_r2fp32()
    stop exit_code

contains

    integer function monadic_r2fp32() result(r)
        ! WARNING : the change in order of operations with feq-parse compared to the compilers
        ! implementation for "fexact" leads to differences that are larger than machine epsilon.
        use FEQParse
        use iso_fortran_env
        implicit none
        integer, parameter :: N = 10
        type(EquationParser) :: f
        character(LEN=1), dimension(1:3) :: independentVars
        character(LEN=1024) :: eqChar
        real(real32), allocatable :: x(:, :, :)
        real(real32), allocatable :: feval(:, :)
        real(real32), allocatable :: fexact(:, :)
        integer :: i, j

        allocate (x(1:N, 1:N, 1:3), &
                  feval(1:N, 1:N), &
                  fexact(1:N, 1:N))

        ! Specify the independent variables
        independentVars = (/'x', 'y', 'z'/)

        ! Specify an equation string that we want to evaluate
        eqChar = 'f = -(x+1)*(y+1)'

        ! Create the EquationParser object
        f = EquationParser(eqChar, independentVars)

        x = 0.0_real32
        do j = 1, N
            do i = 1, N
                x(i, j, 1) = -1.0_real32 + (2.0_real32) / real(N, real32) * real(i - 1, real32)
                x(i, j, 2) = -1.0_real32 + (2.0_real32) / real(N, real32) * real(j - 1, real32)
                fexact(i, j) = -(x(i, j, 1) + 1.0_real32) * (x(i, j, 2) + 1.0_real32)
            end do
        end do

        ! Evaluate the equation
        feval = f%evaluate(x)
        if (maxval(abs(feval - fexact)) <= 10.0_real32 * epsilon(1.0_real32)) then
            r = 0
        else
            r = 1
        end if

        deallocate (x, feval, fexact)

    end function monadic_r2fp32
end program test

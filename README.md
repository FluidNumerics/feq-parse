# Fortran Equation Parser
Copyright 2020 Fluid Numerics LLC

`feqparse` is an equation parser Fortran class that is used to interpret and evaluate functions provided as strings.

## Installation

For a quick installation to `/usr/local/feqparse`,
```
cd build/
cmake ../
make
sudo make install
```
If you'd like to run the provided tests to verify your installation, 
1. Navigate to the `test/` directory underneath the `build/` directory.
```
cd test/
```
2. Use `ctest` to run the provided tests
```
ctest .
```

The above steps install
```
/opt/feqparse/lib/libfeqparse.a
/opt/feqparse/include/FEQParse.mod
```

## Usage

### Demo Program

*Example Makefile*
```
FC = gfortran
FLIBS += -L/opt/feqparse/lib -lfeqparse
FFLAGS += -I/opt/feqparse/include

demo : FEqParseDemo.f90
	${FC} -c FEqParseDemo.f90 ${FFLAGS}
	${FC} FEqParseDemo.o ${FFLAGS} ${FLIBS} -o $@
```

*Example program*
```
PROGRAM FEqParseDemo

USE FEQParse

IMPLICIT NONE

  TYPE(EquationParser) :: f
  CHARACTER(LEN=1), DIMENSION(1:3) :: independentVars
  CHARACTER(LEN=30) :: eqChar
  REAL :: x(1:3)

    ! Specify the independent variables
    independentVars = (/ 'x', 'y', 'z' /)

    ! Specify an equation string that we want to evaluate 
    eqChar = 'f = exp( -(x^2 + y^2 + z^2) )'

    ! Create the EquationParser object
    f = EquationParser(eqChar, independentVars)
   
    ! Evaluate the equation 
    x = (/ 0.0, 0.0, 0.0 /) 
    PRINT*, f % evaluate( x )

    ! Clean up memory
    CALL f % Destruct()


END PROGRAM FEqParseDemo
```

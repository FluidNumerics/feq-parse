# Fortran Equation Parser
Copyright 2020 Fluid Numerics LLC

[![Build Status](https://github.com/fluidnumerics/feq-parse/actions/workflows/ci.yml/badge.svg)](https://github.com/FluidNumerics/feq-parse/actions)

`feqparse` is an equation parser Fortran class that is used to interpret and evaluate functions provided as strings.

[Learn how to contribute to this repository](./CONTRIBUTING.md)

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
/opt/feqparse/lib/libfeqparse-static.a
/opt/feqparse/lib/libfeqparse.so
/opt/feqparse/include/FEQParse.mod
```

### Fortran Package Manager

A [Fortran Package Manager](https://github.com/fortran-lang/fpm) manifest file is also included, so that the library and test cases can be compiled with FPM. For example:

```
fpm build --profile release
fpm test --profile release
```

To use `feq-parse` within your fpm project, add the following to your `fpm.toml` file:
```toml
[dependencies]
feq-parse = { git="https://github.com/FluidNumerics/feq-parse.git" }
```

Or, to use a specific version:

```toml
[dependencies]
feq-parse = { git="https://github.com/FluidNumerics/feq-parse.git", tag = "v1.1.0" }
```

## Usage

!!! note
    All functions in the equation string must start with a `\`

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
    eqChar = 'f = \exp( -(x^2 + y^2 + z^2) )'

    ! Create the EquationParser object
    f = EquationParser(eqChar, independentVars)

    ! Evaluate the equation
    x = (/ 0.0, 0.0, 0.0 /)
    PRINT*, f % evaluate( x )

    ! Clean up memory
    CALL f % Destruct()


END PROGRAM FEqParseDemo
```

## Contributors

* (Maintainer) Joe Schoonover, Fluid Numerics LLC

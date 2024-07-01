# Fortran Equation Parser
Copyright 2020 Fluid Numerics LLC

[![linux-gnu-cmake](https://github.com/fluidnumerics/feq-parse/actions/workflows/linux-gnu-cmake.yml/badge.svg)](https://github.com/FluidNumerics/feq-parse/actions/workflows/linux-gnu-cmake.yml)
[![linux-gnu-fpm](https://github.com/fluidnumerics/feq-parse/actions/workflows/linux-gnu-fpm.yml/badge.svg)](https://github.com/FluidNumerics/feq-parse/actions/workflows/linux-gnu-fpm.yml)


[![linux-intel-cmake](https://github.com/fluidnumerics/feq-parse/actions/workflows/linux-intel-cmake.yml/badge.svg)](https://github.com/FluidNumerics/feq-parse/actions/workflows/linux-intel-cmake.yml)
[![linux-intel-fpm](https://github.com/fluidnumerics/feq-parse/actions/workflows/linux-intel-fpm.yml/badge.svg)](https://github.com/FluidNumerics/feq-parse/actions/workflows/linux-intel-fpm.yml)

[![linux-amdflang-cmake](https://github.com/FluidNumerics/feq-parse/actions/workflows/linux-amdflang-cmake.yaml/badge.svg)](https://github.com/FluidNumerics/feq-parse/actions/workflows/linux-amdflang-cmake.yaml)
[![linux-nvfortran-cmake](https://github.com/FluidNumerics/feq-parse/actions/workflows/linux-nvfortran-cmake.yaml/badge.svg)](https://github.com/FluidNumerics/feq-parse/actions/workflows/linux-nvfortran-cmake.yaml)

[![windows-gnu-cmake](https://github.com/fluidnumerics/feq-parse/actions/workflows/windows-gnu-cmake.yml/badge.svg)](https://github.com/FluidNumerics/feq-parse/actions/workflows/windows-gnu-cmake.yml)
[![windows-gnu-fpm](https://github.com/fluidnumerics/feq-parse/actions/workflows/windows-gnu-fpm.yml/badge.svg)](https://github.com/FluidNumerics/feq-parse/actions/workflows/windows-gnu-fpm.yml)

[![codecov](https://codecov.io/gh/FluidNumerics/feq-parse/graph/badge.svg?token=IBNDDI4MHB)](https://codecov.io/gh/FluidNumerics/feq-parse)

`feq-parse` is an equation parser Fortran class that is used to interpret and evaluate functions provided as strings.

[Learn how to contribute to this repository](./CONTRIBUTING.md)

[**User Documentation**](https://feqparse.fluidnumerics.com/)
[**API Documentation**](https://feqparse.fluidnumerics.com/ford)

## Installation
`feq-parse` can be installed using either CMake, [Fortran Package Manager (fpm)](https://github.com/fortran-lang/fpm), [Spack](https://spack.io), or with [`apt-get`](https://wiki.debian.org/apt-get) on Debian-based distributions.

### Prerequisites
All you need is a Fortran compiler that is compliant with the Fortran 2008 standard and supports C interoperability. You can see which compilers are regularly tested on the [Github actions page](https://github.com/FluidNumerics/feq-parse/actions/workflows/ci.yml). Additionally, the table below lists the [supported compilers](#supported-compilers)

If you are installing with CMake, you will need to have CMake version 3.0.2 or greated


### CMake
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
/usr/local/lib/libfeqparse-static.a
/usr/local/lib/libfeqparse.so
/usr/local/include/feqparse_floatstacks.mod
/usr/local/include/feqparse_functions.mod
/usr/local/include/feqparse_tokenstack.mod
/usr/local/include/feqparse.mod
```

### Fortran Package Manager

A [Fortran Package Manager](https://github.com/fortran-lang/fpm) manifest file is also included, so that the library and test cases can be compiled with FPM. For example:

```
fpm build --profile release
fpm test --profile release
```

You can also run the examples included in the `example/` subdirectory :
```
fpm run --example="*"
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

### Spack
The maintainers of this repository also keep the `feq-parse` spack package up to date with the latest releases. This means you can easily install `feq-parse` from source with the [spack package manager](https://spack.io).

To get started with Spack, if you haven't already
```
git clone https://github.com/spack/spack ~/spack
source ~/spack/share/spack/setup-env.sh
spack compiler find
```

To install the latest version of `feq-parse` with spack,
```
spack install feq-parse
```

To install a specific version of feq-parse with spack, e.g.
```
spack install feq-parse@2.2.2
```

Refer to the [spack documentation](https://spack.readthedocs.io/en/latest/) for further guidance on using Spack.

### `apt-get`
A `.deb` file is provided for each release. If you are using a Debian-based linux distribution (e.g., Debian, Ubuntu, Pop_OS!) and an `x86_64/amd64` architecture, simply download the `.deb` via the [github interface](), or with
```
wget https://github.com/FluidNumerics/feq-parse/releases/download/<version-number>/feq-parse_<version-number>_amd64.deb -P /tmp
```
and then run
```
sudo apt-get install ./feq-parse_<version-number>-1_amd64.deb
```
in the directory in which it was downloaded.

All `.deb` releases are built using **Ubuntu 20.04.06 LTS** with **GNU Fortran 10**.

## Supported Compilers

The following combinations are tested on the main branch of feq-parse:

Name | Version | Platform | Build System | Architecture
--- | --- | --- | --- | --- |
GNU Fortran | 10 | Ubuntu 20.04.6 LTS | `cmake` | x86_64
GNU Fortran | 9, 10, 11, 12 | Ubuntu 22.04.2 LTS | `fpm`, `cmake` | x86_64
GNU Fortran | 13.2.0 | Windows Server 2022 (10.0.20348 Build 1547) (MSYS2) | `fpm`, `cmake` | x86_64
Intel oneAPI (`ifx`)| 2023.2 | Ubuntu 22.04.2 LTS | `fpm`, `cmake` | x86_64
Intel oneAPI classic (`ifort`) | 2021.1 | Ubuntu 22.04.2 LTS | `fpm`, `cmake` | x86_64
Nvidia HPC (`nvfortran`) | 23.11 | Ubuntu 22.04.2 LTS | `cmake` | x86_64

## Usage

### Run examples with fpm
> [!NOTE]
> Examples are now included in the `example/` subdirectory

Included examples
* `scalar_with_scalar_eval.f90` - Creates an equation parser, and evaluates an equation with scalar input and scalar output.
* `array_with_array_eval.f90` - Creates an equation parser, and evaluates an equation with rank 1 array input and rank 1 output.
* `array_with_scalar_eval.f90` - Creates an equation parser, and evaluates an equation with scalar array input and scalar output within a do loop to fill an array of values. This example is to demonstrate the performance difference with using the array evaluation.
* `gaussian_scalar_multivar.f90` -  Creates an equation parser, and evaluates an equation with scalar input and scalar output but with multiple independent variables (much like the example shown below).
* `scalar_function_product.f90` - Creates an equation parser, and evaluates an equation with scalar array input and scalar output, and demonstrates multiplication of two functions.

To run the included examples with the fortran package manager,
```
fpm run --example "*"
```

### Simple example with Makefile
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

END PROGRAM FEqParseDemo
```

## Contributors

* (Maintainer) Joe Schoonover, Fluid Numerics LLC

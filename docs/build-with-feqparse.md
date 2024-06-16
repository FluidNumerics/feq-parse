# Build your application with feq-parse

Once feq-parse is installed, you can use the library to start creating your own Fortran applications with dynamic equation support! Below, we provide example Makefiles and CMakeLists.txt to get your started. You can peruse the [feq-parse/example](https://github.com/FluidNumerics/feq-parse/tree/master/example) for demonstrations of how you can use `feq-parse` in your project.


## Build

When building an application with feqparse, you need to specify linker and includes flags to your Fortran compiler so that it can find the feqparse library and Fortran `.mod` files. Below are examples for doing this with a Makefile and with a CMake build system.

### Makefile
```
FC?=gfortran # This needs to be the same Fortran compiler that was used to build feq-parse
FEQPARSE_ROOT?=/usr  # Modify this line or set FEQPARSE_ROOT environment variable to the root installation path for feq-parse

FEQPARSE_LIB=-L${FEQPARSE_ROOT}/lib/ -lfeqparse
FEQPARSE_INCLUDE=-I${FEQPARSE_ROOT}/include

app: app.o
    ${FC} ${FEQPARSE_LIB} ${FEQPARSE_INCLUDE} app.o -o $@

app.o:
    ${FC} -c ${FEQPARSE_LIB} ${FEQPARSE_INCLUDE} app.f90 -o $@
```

### CMakeLists.txt

```
cmake_minimum_required(VERSION 3.21)
cmake_policy(VERSION 3.21...3.27)

project(myproject VERSION 1.0.0
        DESCRIPTION "A totally useful application"
        LANGUAGES Fortran)


# FEQ-Parse
find_library(FEQPARSE_LIBRARIES NAMES feqparse REQUIRED)
find_path(FEQPARSE_INCLUDE_DIRS feqparse.mod)

# After you declare your build targets, you can use:
#
#    target_link_libraries(target-name PRIVATE ${FEQPARSE_LIBRARIES})
#
# to link your target to the feq-parse library, and you can use :
#
#     target_include_directories(target-name PRIVATE ${FEQPARSE_INCLUDE_DIRS})
#
# to add the necessary includes flags to the feq-parse `.mod` files.
#
```


CMAKE_MINIMUM_REQUIRED(VERSION 3.0.2)
# Default compiler gfortran, otherwise cmake Fortran package will set it to f95
# You can override the compiler with cmake -DFEQPARSE_COMPILER=
set(FEQPARSE_COMPILER "/usr/bin/gfortran" CACHE STRING "Fortran Compiler to build feqparse")
set(FEQPARSE_AR "/usr/bin/gcc-ar" CACHE STRING "Static archive command")
set(FEQPARSE_RANLIB "/usr/bin/gcc-ranlib" CACHE STRING "ranlib used to create Static archive")
set(FEQPARSE_COMPILER_FLAGS "-std=f2003 -ffree-form -ffree-line-length-none -cpp" CACHE STRING "Compiler flags to build feqparse")
message("-- FEQPARSE -------------  cmake START -------------------")
message("-- FEQPARSE_COMPILER:       ${FEQPARSE_COMPILER}")
message("-- FEQPARSE_AR:             ${FEQPARSE_AR}")
message("-- FEQPARSE_RANLIB:         ${FEQPARSE_RANLIB}")
message("-- FEQPARSE_COMPILER_FLAGS: ${FEQPARSE_COMPILER_FLAGS}")
message("-- FEQPARSE_BUILD_TYPE:     ${FEQPARSE_BUILD_TYPE}")
message("-- FEQPARSE_INSTALL_DIR:    ${FEQPARSE_INSTALL_DIR}")
message("-- FEQPARSE ----------------------------------------------")

set(CMAKE_Fortran_COMPILER_INIT ${FEQPARSE_COMPILER})
PROJECT(feqparse Fortran C)

SET(CMAKE_BUILD_TYPE ${FEQPARSE_BUILD_TYPE})
SET(VERSION 0.0.0)
SET(CMAKE_AR ${FEQPARSE_AR})
SET(CMAKE_RANLIB ${FEQPARSE_RANLIB})
SET(CMAKE_INSTALL_PREFIX ${FEQPARSE_INSTALL_DIR})
message("-- CMAKE_INSTALL_PREFIX:${CMAKE_INSTALL_PREFIX}")

INCLUDE(FortranCInterface)
FortranCInterface_VERIFY()
IF(NOT FortranCInterface_VERIFIED_C)
	MESSAGE(FATAL_ERROR "Fortran compiler must support C Interface")
ENDIF(NOT FortranCInterface_VERIFIED_C)
	
IF(NOT CMAKE_Fortran_COMPILER_SUPPORTS_F90)
    MESSAGE(FATAL_ERROR "Fortran compiler does not support F90")
ENDIF(NOT CMAKE_Fortran_COMPILER_SUPPORTS_F90)

ADD_SUBDIRECTORY(${CMAKE_SOURCE_DIR}/src)
#ADD_SUBDIRECTORY(${CMAKE_SOURCE_DIR}/test)

message("-- FEQPARSE -------------  cmake DONE --------------------")

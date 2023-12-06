CMAKE_MINIMUM_REQUIRED(VERSION 3.0.2)

set(CMAKE_VERBOSE_MAKEFILE ON)

PROJECT(feqparse Fortran C)
SET(VERSION 0.0.0)

INCLUDE(FortranCInterface)
FortranCInterface_VERIFY()
IF(NOT FortranCInterface_VERIFIED_C)
	MESSAGE(FATAL_ERROR "Fortran compiler must support C Interface")
ENDIF(NOT FortranCInterface_VERIFIED_C)
	
IF(NOT CMAKE_Fortran_COMPILER_SUPPORTS_F90)
    MESSAGE(FATAL_ERROR "Fortran compiler does not support F90")
ENDIF(NOT CMAKE_Fortran_COMPILER_SUPPORTS_F90)

# Default Fortran compiler flags
# Fortran compiler flags

if( "${CMAKE_Fortran_COMPILER_ID}" STREQUAL "GNU" )
    set( CMAKE_Fortran_FLAGS "-cpp -ffree-line-length-512" )
    set( CMAKE_Fortran_FLAGS_DEBUG "-g -O0 -C -Wall -fbounds-check -fbacktrace -ffpe-trap=invalid,zero,overflow" )
    set( CMAKE_Fortran_FLAGS_COVERAGE "${CMAKE_Fortran_FLAGS_DEBUG} --coverage")
    set( CMAKE_Fortran_FLAGS_PROFILE "-pg -O3")
    set( CMAKE_Fortran_FLAGS_RELEASE "-O3" )
    set( CMAKE_C_FLAGS_COVERAGE "-g -O0 --coverage")

elseif( "${CMAKE_Fortran_COMPILER_ID}" STREQUAL "Intel" )
    set( CMAKE_Fortran_FLAGS "-fpp" )
    set( CMAKE_Fortran_FLAGS_DEBUG "-O0 -g -debug all -check all -traceback" )
    set( CMAKE_Fortran_FLAGS_COVERAGE "${CMAKE_Fortran_FLAGS_DEBUG} -prof-gen=srcpos")
    set( CMAKE_Fortran_FLAGS_PROFILE "-O3")
    set( CMAKE_Fortran_FLAGS_RELEASE "-O3" )
    set( CMAKE_C_FLAGS_COVERAGE "-g -O0")

elseif( "${CMAKE_Fortran_COMPILER_ID}" STREQUAL "IntelLLVM" )
    set( CMAKE_Fortran_FLAGS "-fpp" )
    set( CMAKE_Fortran_FLAGS_DEBUG "-debug all" )
    set( CMAKE_Fortran_FLAGS_COVERAGE "${CMAKE_Fortran_FLAGS_DEBUG}") # Code coverage not available with ifx
    set( CMAKE_Fortran_FLAGS_PROFILE "-O3")
    set( CMAKE_Fortran_FLAGS_RELEASE "-O3" )
    set( CMAKE_C_FLAGS_COVERAGE "-g -O0")
endif()

ADD_SUBDIRECTORY(${CMAKE_SOURCE_DIR}/src)
ADD_SUBDIRECTORY(${CMAKE_SOURCE_DIR}/test)

message("-- FEQPARSE -------------  cmake DONE --------------------")

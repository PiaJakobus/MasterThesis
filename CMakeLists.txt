cmake_minimum_required(VERSION 2.8 FATAL_ERROR)

project(TOV_solver)

enable_language(Fortran)

set(CMAKE_Fortran_MODULE_DIRECTORY ${PROJECT_BINARY_DIR}/modules)#mod files
set(CMAKE_BINARY_DIR ${CMAKE_SOURCE_DIR}/build) #?
set(EXECUTABLE_OUTPUT_PATH ${CMAKE_BINARY_DIR}) #exec. file

add_executable(
    exec
    src/main.f90
    src/parameters.f90
    src/eos_tpp.f90
    src/ode.f90
    src/rk.f90
    )

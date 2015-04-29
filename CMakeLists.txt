cmake_minimum_required ( VERSION 2.8.8 FATAL_ERROR )

# Set CMake language to Fortran
enable_language ( Fortran )

# Define object binary directory
set ( BIN_DIR ${CMAKE_BINARY_DIR}/bin )
set ( LIB_DIR ${CMAKE_BINARY_DIR}/lib )

# place executables in the binary root directory
set ( EXECUTABLE_OUTPUT_PATH ${CMAKE_BINARY_DIR} )

# Define lib as the module directory
set ( CMAKE_Fortran_MODULE_DIRECTORY ${LIB_DIR} )


# Define string utility module source directory
set ( LL_SOURCE_DIR ${CMAKE_SOURCE_DIR}/src )

# Define module source files
set ( LL_SOURCE 
  ${LL_SOURCE_DIR}/linked_list_type.f90
)


# Define tests directory
set ( LL_TESTS_SOURCE_DIR ${CMAKE_SOURCE_DIR}/tests )

# Define tests source files
set ( LL_TESTS_SOURCE
  ${LL_TESTS_SOURCE_DIR}/tests.f90
  ${LL_SOURCE}
)


# Define test executable name
set ( LL_TESTS_EXE linked_list_tests )

# Define tests executable
add_executable ( ${LL_TESTS_EXE} ${LL_TESTS_SOURCE} )

cmake_minimum_required(VERSION 3.19)

project(gsiutils
        VERSION 1.0.0
        LANGUAGES C Fortran)

list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake")
set(CMAKE_DIRECTORY_LABELS ${PROJECT_NAME})

include(GNUInstallDirs)

if(NOT CMAKE_BUILD_TYPE MATCHES "^(Debug|Release|RelWithDebInfo|MinSizeRel)$")
  message(STATUS "Setting build type to 'Release' as none was specified.")
  set(CMAKE_BUILD_TYPE
      "Release"
      CACHE STRING "Choose the type of build." FORCE)
  set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS "Debug" "Release" "MinSizeRel" "RelWithDebInfo")
endif()

if(NOT CMAKE_C_COMPILER_ID MATCHES "^(GNU|Intel|Clang|AppleClang)$")
  message(WARNING "${CMAKE_C_COMPILER_ID} is not supported.")
endif()

if(NOT CMAKE_Fortran_COMPILER_ID MATCHES "^(GNU|Intel)$")
  message(WARNING "${CMAKE_Fortran_COMPILER_ID} is not supported.")
endif()

# User options
option(OPENMP "Enable OpenMP Threading" OFF)
option(ENABLE_MKL "Use MKL for LAPACK implementation (if available)" ON)

# Build options
option(BUILD_UTIL_ALL "Build all utilities" OFF)
option(BUILD_UTIL_AERODA "Build Aerosol DA utilities" OFF)
option(BUILD_UTIL_COV_CALC "Build Correlated Observation Error Utility" OFF)
option(BUILD_UTIL_EFSOI "Build EFSOI Utilities" OFF)
option(BUILD_UTIL_ENKF_GFS "Build EnKF GFS Utilities" OFF)
option(BUILD_UTIL_ENKF_ARW "Build EnKF ARW Utilities" OFF)
option(BUILD_UTIL_NCIO "Build NetCDF IO Utilities" OFF)
option(BUILD_UTIL_BUFR "Build BUFR Utilities" ON)
option(BUILD_UTIL_COM "Build community utilities" OFF)
option(BUILD_UTIL_BKGERR "Build Background Error Utilities" OFF)
option(BUILD_UTIL_ETC "Build Miscellaneous Utilities" OFF)

# If building all utils, then force all options to ON
# Exception is BUILD_UTIL_ENKF_ARW, since it is incompatible with GSI_MODE=GFS
if (BUILD_UTIL_ALL)
  set(BUILD_UTIL_AERODA ON CACHE BOOL "Build Aerosol DA utilities" FORCE)
  set(BUILD_UTIL_COV_CALC ON CACHE BOOL "Build Correlated Observation Error Utility" FORCE)
  set(BUILD_UTIL_EFSOI ON CACHE BOOL "Build EFSOI Utilities" FORCE)
  set(BUILD_UTIL_ENKF_GFS ON CACHE BOOL "Build EnKF GFS Utilities" FORCE)
  set(BUILD_UTIL_ENKF_ARW OFF CACHE BOOL "Build EnKF ARW Utilities" FORCE)
  set(BUILD_UTIL_NCIO ON CACHE BOOL "Build NetCDF IO Utilities" FORCE)
  set(BUILD_UTIL_BUFR ON CACHE BOOL "Build BUFR Utilities" FORCE)
  set(BUILD_UTIL_COM ON CACHE BOOL "Build community utilities" FORCE)
  set(BUILD_UTIL_BKGERR ON CACHE BOOL "Build Background Error Utilities" FORCE)
  set(BUILD_UTIL_ETC ON CACHE BOOL "Build Miscellaneous Utilities" FORCE)
endif()

# Echo user options
message(STATUS "Utils: OPENMP ................. ${OPENMP}")
message(STATUS "Utils: ENABLE_MKL ............. ${ENABLE_MKL}")
message(STATUS "Utils: BUILD_UTIL_AERODA ...... ${BUILD_UTIL_AERODA}")
message(STATUS "Utils: BUILD_UTIL_COV_CALC .... ${BUILD_UTIL_COV_CALC}")
message(STATUS "Utils: BUILD_UTIL_EFSOI ....... ${BUILD_UTIL_EFSOI}")
message(STATUS "Utils: BUILD_UTIL_ENKF_GFS .... ${BUILD_UTIL_ENKF_GFS}")
message(STATUS "Utils: BUILD_UTIL_ENKF_ARW .... ${BUILD_UTIL_ENKF_ARW}")
message(STATUS "Utils: BUILD_UTIL_NCIO ........ ${BUILD_UTIL_NCIO}")
message(STATUS "Utils: BUILD_UTIL_BUFR ........ ${BUILD_UTIL_BUFR}")
message(STATUS "Utils: BUILD_UTIL_COM ......... ${BUILD_UTIL_COM}")
message(STATUS "Utils: BUILD_UTIL_BKGERR ...... ${BUILD_UTIL_BKGERR}")
message(STATUS "Utils: BUILD_UTIL_ETC ......... ${BUILD_UTIL_ETC}")

# Dependencies
if(ENABLE_MKL)
  find_package(MKL QUIET)
endif()
if(MKL_FOUND)
  set(LAPACK_LIBRARIES ${MKL_LIBRARIES})
else()
  set(ENABLE_MKL OFF CACHE INTERNAL "GSI Utils: Disable MKL since it was NOT FOUND")
  find_package(LAPACK REQUIRED)
endif()
find_package(MPI REQUIRED)
find_package(NetCDF REQUIRED Fortran)
if(OPENMP)
  find_package(OpenMP REQUIRED)
endif()

# Optional dependencies (if not found, some utilities will not be built)
find_package(ncdiag QUIET)
find_package(gsi QUIET)
find_package(enkf QUIET)

# NCEPLibs dependencies
find_package(bacio REQUIRED)
find_package(sigio REQUIRED)
find_package(sfcio REQUIRED)
find_package(nemsio REQUIRED)
find_package(ncio REQUIRED)
find_package(sp REQUIRED)
find_package(ip REQUIRED)
find_package(w3emc REQUIRED)
find_package(bufr REQUIRED)
find_package(wrf_io REQUIRED)

# See https://github.com/NOAA-EMC/NCEPLIBS-nemsio/pull/22
target_link_libraries(nemsio::nemsio INTERFACE w3emc::w3emc_d bacio::bacio_4)

# Get compiler flags for the utilities
include(gsiutils_compiler_flags)

add_subdirectory(src)


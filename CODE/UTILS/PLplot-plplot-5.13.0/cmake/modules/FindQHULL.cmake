# Find qhull header and library.
#

# This module defines the following uncached variables:
#  QHULL_FOUND, if false, do not try to use qhull.
#  QHULL_INCLUDE_DIRS, where to find libqhull/qhull_a.h OR qhull/qhull_a.h.
#  HAS_LIBQHULL_INCLUDE
#    if true use the libqhull/qhull_a.h form of header,
#    if false use the qhull/qhull_a.h form of header.
#  QHULL_LIBRARIES, the libraries to link against to use the qhull library
#  QHULL_LIBRARY_DIRS, the directory where the qhull library is found.

include(FindPackageHandleStandardArgs)

find_file(QHULL_HEADER_FILE NAMES qhull/qhull_a.h libqhull/qhull_a.h)
if(QHULL_HEADER_FILE)
  # message(STATUS "QHULL_HEADER_FILE = ${QHULL_HEADER_FILE}")
  if(QHULL_HEADER_FILE MATCHES "libqhull")
    set(HAS_LIBQHULL_INCLUDE ON)
  else(QHULL_HEADER_FILE MATCHES "libqhull")
    set(HAS_LIBQHULL_INCLUDE OFF)
  endif(QHULL_HEADER_FILE MATCHES "libqhull")
  # Determine grandparent directory of header file
  get_filename_component(QHULL_INCLUDE_DIRS ${QHULL_HEADER_FILE} DIRECTORY)
  get_filename_component(QHULL_INCLUDE_DIRS ${QHULL_INCLUDE_DIRS} DIRECTORY)
elseif(NOT QHULL_HEADER_FILE AND NOT QHULL_FIND_QUIETLY)
  message(STATUS "Neither qhull/qhull_a.h header nor libqhull/qhull_a.h could be found")
endif(QHULL_HEADER_FILE)

find_library(QHULL_LIBRARY qhull)
if( QHULL_LIBRARY )
  get_filename_component(QHULL_LIBRARY_DIRS ${QHULL_LIBRARY} DIRECTORY)
  set(QHULL_LIBRARIES ${QHULL_LIBRARY})
elseif(NOT QHULL_LIBRARY AND NOT QHULL_FIND_QUIETLY)
  message(STATUS "qhull library could not be found")
endif(QHULL_LIBRARY)

find_package_handle_standard_args(QHULL
  REQUIRED_VARS QHULL_INCLUDE_DIRS QHULL_LIBRARIES QHULL_LIBRARY_DIRS
  )

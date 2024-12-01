# Check if thermopack has been compiled and installed to the proper location for inclusion in other projects.
# Result variables:
#   THERMOPACK_INSTALLED : "TRUE" if thermopack is installed, "FALSE" otherwise
#   THERMOPACK_ROOT : Path to directory containing this file
#   THERMOPACK_LIB : Path to thermopack dynamic library
#   THERMOPACK_INCLUDE : Path to thermopack C++ headers
#   thermopack : Imported shared library target with headers


message(STATUS "ThermoPack found at: ${CMAKE_CURRENT_LIST_DIR}")
set(THERMOPACK_LIB ${CMAKE_CURRENT_LIST_DIR}/installed/libthermopack${CMAKE_SHARED_LIBRARY_SUFFIX})
set(THERMOPACK_ROOT ${CMAKE_CURRENT_LIST_DIR})

if(NOT EXISTS ${THERMOPACK_LIB})
    unset(THERMOPACK_LIB)
    set(THERMOPACK_INSTALLED FALSE)
    message(STATUS "ThermoPack has not been properly installed.")
    message(STATUS "Setting ThermoPack cmake variables ...")
    message(STATUS "THERMOPACK_INSTALLED: ${THERMOPACK_INSTALLED}")
    message(STATUS "THERMOPACK_ROOT: ${THERMOPACK_ROOT}")
    return()
endif()

set(THERMOPACK_INSTALL_DIR ${CMAKE_CURRENT_LIST_DIR}/installed)
set(THERMOPACK_STATIC_LIB ${THERMOPACK_INSTALL_DIR}/libthermopack${CMAKE_STATIC_LIBRARY_SUFFIX})
if(NOT EXISTS ${THERMOPACK_STATIC_LIB})
    set(THERMOPACK_STATIC_LIB "")
endif()

set(THERMOPACK_FOUND TRUE)
set(THERMOPACK_INSTALLED TRUE)
set(THERMOPACK_INCLUDE ${CMAKE_CURRENT_LIST_DIR}/addon/cppThermopack)

message(STATUS "Setting ThermoPack cmake variables ...")
message(STATUS "THERMOPACK_INSTALLED: ${THERMOPACK_INSTALLED}")
message(STATUS "THERMOPACK_ROOT: ${THERMOPACK_ROOT}")
message(STATUS "THERMOPACK_INSTALL_DIR: ${THERMOPACK_INSTALL_DIR}")
message(STATUS "THERMOPACK_LIB: ${THERMOPACK_LIB}")
message(STATUS "THERMOPACK_STATIC_LIB: ${THERMOPACK_STATIC_LIB}")
message(STATUS "THERMOPACK_INCLUDE: ${THERMOPACK_INCLUDE}")

if(NOT TARGET thermopack)
    add_library(thermopack SHARED IMPORTED)
    set_target_properties(thermopack PROPERTIES 
                            IMPORTED_LOCATION ${THERMOPACK_LIB}
                            INTERFACE_INCLUDE_DIRECTORIES ${THERMOPACK_INCLUDE})
    if(MSVC)
        set_target_properties(thermopack PROPERTIES
                                IMPORTED_IMPLIB ${THERMOPACK_STATIC_LIB})
    endif()
    message(STATUS "ThermoPack exported target: thermopack")
endif()
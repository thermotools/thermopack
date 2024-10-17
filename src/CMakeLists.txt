# Set FORTRAN compile flags for thermopack and lapack
set(CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS_RELEASE} ${tp_optim_flags}")
set(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} ${tp_debug_flags}")

file(GLOB SOURCES ${CMAKE_CURRENT_SOURCE_DIR}/*.f90)
list(REMOVE_ITEM SOURCES ${CMAKE_CURRENT_SOURCE_DIR}/thermopack.f90)
set(TREND_DIR ${CMAKE_CURRENT_SOURCE_DIR}/../addon/trend_interface/include)
set(TREND ${TREND_DIR}/trend_interface.f95)

if(NOT MSVC)
    find_package(LAPACK REQUIRED)
else()
    if(POLICY CMP0077)
        set(CMAKE_POLICY_DEFAULT_CMP0077 NEW)
    endif()
    # Disable single/complex/complex16 as it is not used by thermopack
    set(BUILD_SINGLE OFF)
    set(BUILD_COMPLEX OFF)
    set(BUILD_COMPLEX16 OFF)
    add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/../external/lapack "lapack")
    # Add flags not comatible with lapack
    set(CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS_RELEASE} /real-size:64 ")
    set(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} /real-size:64 /warn:all,noexternal /check:all,noarg_temp_created,nopointers")
endif()

add_library(thermopack_obj OBJECT ${SOURCES})
target_include_directories(thermopack_obj PUBLIC ${TREND_DIR})
if(MSVC)
    set(CMAKE_MSVC_RUNTIME_LIBRARY "MultiThreaded$<$<CONFIG:Debug>:Debug>")
    add_library(thermopack SHARED ${SOURCES})
    set_target_properties(thermopack PROPERTIES Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/dynamic OUTPUT_NAME libthermopack)
    set(CMAKE_SHARED_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS} /DEF:${CMAKE_SOURCE_DIR}/MSVStudio/thermopack.def")
    target_link_options(thermopack PRIVATE "/NODEFAULTLIB:libmmd" "/NODEFAULTLIB:MSVCRT" "/NODEFAULTLIB:libifcoremd" "/NODEFAULTLIB:svml_dispmd")
    target_include_directories(thermopack PRIVATE ${TREND_DIR})
else(MSVC)
    if(APPLE)
        set(tp_linker_flags "-exported_symbols_list ${CMAKE_CURRENT_SOURCE_DIR}/../libthermopack_export.symbols")
        set(CMAKE_SHARED_LINKER_FLAGS "-static-libgfortran -static-libquadmath")
    else(APPLE)
        set(tp_linker_flags "-Wl,--version-script=${CMAKE_CURRENT_SOURCE_DIR}/../libthermopack_export.version")
    endif(APPLE)
    add_library(thermopack SHARED $<TARGET_OBJECTS:thermopack_obj>)
    target_link_options(thermopack PRIVATE ${tp_linker_flags})
    set_target_properties(thermopack PROPERTIES Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})
    target_link_libraries(thermopack ${LAPACK_LIBRARIES})
endif(MSVC)

add_library(thermopack_static STATIC $<TARGET_OBJECTS:thermopack_obj>)
set_target_properties(thermopack_static PROPERTIES 
                                    Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
                                    OUTPUT_NAME thermopack)
target_link_libraries(thermopack_static PRIVATE ${LAPACK_LIBRARIES})

install(TARGETS thermopack DESTINATION ${CMAKE_CURRENT_SOURCE_DIR}/../installed/)
install(TARGETS thermopack_static DESTINATION ${CMAKE_CURRENT_SOURCE_DIR}/../installed/)
install(TARGETS thermopack DESTINATION ${CMAKE_CURRENT_SOURCE_DIR}/../addon/pycThermopack/thermopack)

add_executable(run_thermopack ${CMAKE_CURRENT_SOURCE_DIR}/thermopack.f90)
target_include_directories(run_thermopack PUBLIC ${CMAKE_CURRENT_BINARY_DIR})

if(NOT MSVC)
    target_link_libraries(run_thermopack thermopack_static)
else()
    target_link_options(run_thermopack PRIVATE "/NODEFAULTLIB:MSVCRT")
    target_link_libraries(thermopack PUBLIC lapack blas)
    target_link_libraries(run_thermopack thermopack_static lapack blas)
endif()

install(TARGETS run_thermopack DESTINATION ${CMAKE_CURRENT_SOURCE_DIR}/../installed/)

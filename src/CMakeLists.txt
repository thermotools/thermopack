set(CMAKE_Fortran_FLAGS ${tp_fortran_flags})

file(GLOB SOURCES ${CMAKE_CURRENT_SOURCE_DIR}/*.f90)
list(REMOVE_ITEM SOURCES ${CMAKE_CURRENT_SOURCE_DIR}/thermopack.f90)
set(TREND_DIR ${CMAKE_CURRENT_SOURCE_DIR}/../addon/trend_interface/include)
set(TREND ${TREND_DIR}/trend_interface.f95)

find_package(LAPACK REQUIRED)

add_library(thermopack SHARED ${SOURCES})
set_target_properties(thermopack PROPERTIES 
                        Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})
target_include_directories(thermopack PUBLIC ${TREND_DIR})
target_link_libraries(thermopack ${LAPACK_LIBRARIES})

install(TARGETS thermopack DESTINATION ${CMAKE_CURRENT_SOURCE_DIR}/../addon/pycThermopack/thermopack)
install(TARGETS thermopack DESTINATION ${CMAKE_CURRENT_SOURCE_DIR}/../addon/cppThermopack)

add_executable(run_thermopack ${CMAKE_CURRENT_SOURCE_DIR}/thermopack.f90)
target_link_libraries(run_thermopack thermopack)
target_include_directories(run_thermopack PUBLIC ${CMAKE_CURRENT_BINARY_DIR})

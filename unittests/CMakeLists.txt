
set(CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS_RELEASE} ${tp_optim_flags}")
set(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} ${tp_debug_flags}")

enable_testing()
file(GLOB UNITTEST_SRC ${CMAKE_CURRENT_SOURCE_DIR}/*.pf)
list(REMOVE_ITEM UNITTEST_SRC ${CMAKE_CURRENT_SOURCE_DIR}/test_trend.pf)
add_pfunit_ctest(unittests TEST_SOURCES ${UNITTEST_SRC} LINK_LIBRARIES thermopack_static lapack blas)
target_include_directories(unittests PRIVATE ${CMAKE_BINARY_DIR}/thermopack)

if (NOT MSVC AND NOT APPLE)
    set_target_properties(unittests PROPERTIES 
                            INSTALL_RPATH ${CMAKE_CURRENT_SOURCE_DIR}/../installed
                            INSTALL_RPATH_USE_LINK_PATH TRUE)
endif()

install(TARGETS unittests DESTINATION ${CMAKE_CURRENT_SOURCE_DIR}/../installed/)

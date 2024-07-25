
set(CMAKE_Fortran_FLAGS ${tp_fortran_flags})

enable_testing()
file(GLOB UNITTEST_SRC ${CMAKE_CURRENT_SOURCE_DIR}/*.pf)
list(REMOVE_ITEM UNITTEST_SRC ${CMAKE_CURRENT_SOURCE_DIR}/test_trend.pf)
add_pfunit_ctest(run_unittests TEST_SOURCES ${UNITTEST_SRC} LINK_LIBRARIES thermopack)
target_include_directories(run_unittests PRIVATE ${CMAKE_BINARY_DIR}/thermopack)
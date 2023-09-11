file(GLOB cpp_wrapper_src *.cpp *.f90)
list(REMOVE_ITEM cpp_wrapper_src ${CMAKE_CURRENT_SOURCE_DIR}/pybind11_bindings.cpp)

add_library(demopack_cpp ${cpp_wrapper_src})
target_include_directories(demopack_cpp PUBLIC ../build/fortran_lib)
target_link_libraries(demopack_cpp PUBLIC demopack)

pybind11_add_module(libdemo pybind11_bindings.cpp)
target_link_libraries(libdemo PUBLIC demopack_cpp demopack)

add_custom_command(
  TARGET libdemo POST_BUILD
  COMMAND ${CMAKE_COMMAND} -E copy "${CMAKE_CURRENT_BINARY_DIR}/libdemo.*.so"
  "${CMAKE_CURRENT_BINARY_DIR}/../py_wrapper/")

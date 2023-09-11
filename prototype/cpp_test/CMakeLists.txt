# add_library(libgfortran SHARED IMPORTED GLOBAL)
# set_target_properties(libgfortran PROPERTIES IMPORTED_LOCATION /opt/homebrew/lib/gcc/13/libgfortran.dylib)

add_executable(cpp_main main.cpp)
target_include_directories(cpp_main PUBLIC ../build/fortran_lib ../cpp_wrapper)
target_link_libraries(cpp_main PUBLIC demopack demopack_cpp)

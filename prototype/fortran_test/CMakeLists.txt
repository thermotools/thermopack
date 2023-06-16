# add_library(libgfortran SHARED IMPORTED GLOBAL)
# set_target_properties(libgfortran PROPERTIES IMPORTED_LOCATION /opt/homebrew/lib/gcc/13/libgfortran.dylib)

add_executable(fortran_main main.f90)
target_include_directories(fortran_main PUBLIC ../build/fortran_lib)
target_link_libraries(fortran_main PUBLIC demopack libgfortran)
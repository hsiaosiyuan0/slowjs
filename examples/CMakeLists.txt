add_library(examples_fib SHARED "fib.c")

# the lib to be loaded by qjs should have the name without prefix `lib` and with
# suffix `.so`
set_target_properties(
  examples_fib
  PROPERTIES PREFIX ""
             SUFFIX ".so"
             OUTPUT_NAME "fib"
             LIBRARY_OUTPUT_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}")
target_compile_definitions(examples_fib PRIVATE JS_SHARED_LIBRARY=y)
target_link_options(examples_fib PRIVATE -shared -undefined dynamic_lookup)

add_library(examples_point SHARED "point.c")
set_target_properties(
  examples_point
  PROPERTIES PREFIX ""
             SUFFIX ".so"
             OUTPUT_NAME "point"
             LIBRARY_OUTPUT_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}")
target_compile_definitions(examples_point PRIVATE JS_SHARED_LIBRARY=y)
target_link_options(examples_point PRIVATE -shared -undefined dynamic_lookup)

add_custom_target(examples DEPENDS examples_fib examples_point)

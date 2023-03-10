add_library(tests SHARED "bjson.c")

# the lib to be loaded by qjs should have the name without prefix `lib` and with
# suffix `.so`
set_target_properties(
  tests
  PROPERTIES PREFIX ""
             SUFFIX ".so"
             OUTPUT_NAME "bjson"
             LIBRARY_OUTPUT_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}")
target_compile_definitions(tests PRIVATE JS_SHARED_LIBRARY=y)

# `-undefined dynamic_lookup` will get an annoy warning on OSX greater then 12:
#
# ~~~
# ld: warning: -undefined dynamic_lookup may not work with chained fixups
# ~~~
#
# get more info from: https://github.com/python/cpython/issues/97524
target_link_options(tests PRIVATE -shared -undefined dynamic_lookup)

add_dependencies(tests qjs examples)

set(QJS "$<TARGET_FILE:qjs>")

macro(qjs_test)
  add_test(NAME test_${ARGV0}
           COMMAND ${QJS} ${ARGV1}
                   "${PROJECT_SOURCE_DIR}/tests/test_${ARGV0}.js")
endmacro()

enable_testing()

qjs_test(closure)
qjs_test(language)
qjs_test(builtin)
qjs_test(loop)
qjs_test(std)
qjs_test(worker)

if(QJS_CONFIG_BIGNUM)
  qjs_test(bjson "--bignum")
  qjs_test(op_overloading "--bignum")
  qjs_test(bignum "--bignum")
  qjs_test(qjscalc "--qjscalc")
else()
  qjs_test(bjson)
endif()

add_custom_target(
  run-tests
  COMMAND ctest -C $<CONFIG> --output-on-failure
  DEPENDS tests
  JOB_POOL console)

add_custom_target(
  microbench
  COMMAND ${QJS} "${CMAKE_CURRENT_SOURCE_DIR}/microbench.js"
  DEPENDS qjs
  JOB_POOL console)

add_custom_target(
  test-col
  COMMAND ${QJS} "${PROJECT_SOURCE_DIR}/tests/test-col/run-tests.js"
  DEPENDS qjs
  JOB_POOL console)

add_custom_target(
  test-op_loc
  COMMAND ${QJS} "${PROJECT_SOURCE_DIR}/tests/test-op_loc/run-tests.js"
  DEPENDS qjs
  JOB_POOL console)

add_subdirectory(run-test262)

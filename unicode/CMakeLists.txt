add_executable(unicode_gen "unicode_gen.c")
target_include_directories(quickjs PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}")

target_link_libraries(unicode_gen qjslibs)

set(UNICODE_GEN "${PROJECT_BINARY_DIR}/unicode/unicode_gen")

add_custom_target(
  run_unicode_gen
  COMMAND ${UNICODE_GEN} "${CMAKE_CURRENT_SOURCE_DIR}/data"
          "${PROJECT_SOURCE_DIR}/libs/libunicode-table.h"
  DEPENDS unicode_gen)

add_executable(qjs "qjs.c")
target_link_libraries(qjs quickjs)

execute_process(
  COMMAND cat "${PROJECT_SOURCE_DIR}/VERSION"
  OUTPUT_VARIABLE CONFIG_VERSION
  OUTPUT_STRIP_TRAILING_WHITESPACE)

target_compile_definitions(qjs PRIVATE CONFIG_VERSION="${CONFIG_VERSION}")

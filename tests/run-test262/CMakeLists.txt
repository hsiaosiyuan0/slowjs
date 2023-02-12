add_executable(run-test262 "run-test262.c")
target_link_libraries(run-test262 quickjs)

execute_process(
  COMMAND cat "${PROJECT_SOURCE_DIR}/VERSION"
  OUTPUT_VARIABLE CONFIG_VERSION
  OUTPUT_STRIP_TRAILING_WHITESPACE)

target_compile_definitions(run-test262
                           PRIVATE CONFIG_VERSION="${CONFIG_VERSION}")

add_dependencies(run-test262 quickjs)

set(RUN_TEST262 "$<TARGET_FILE:run-test262>")

add_custom_target(
  run-test262-test2-default
  COMMAND time ${RUN_TEST262} -m -c "${CMAKE_CURRENT_SOURCE_DIR}/test262.conf"
  DEPENDS run-test262
  JOB_POOL console)

add_custom_target(
  run-test262-test2
  COMMAND time ${RUN_TEST262} -m -c "${CMAKE_CURRENT_SOURCE_DIR}/test262.conf"
          -a
  DEPENDS run-test262
  JOB_POOL console)

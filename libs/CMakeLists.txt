file(GLOB_RECURSE SOURCES "${CMAKE_CURRENT_SOURCE_DIR}/*.c")
add_library(libs ${SOURCES})

if(LINUX)
  # `sighandler_t` is in GNU extesion which requires `_GNU_SOURCE` to be
  # defined, more:
  # https://manpages.ubuntu.com/manpages/bionic/man2/signal.2.html
  target_compile_definitions(libs PUBLIC _GNU_SOURCE=y)
endif()

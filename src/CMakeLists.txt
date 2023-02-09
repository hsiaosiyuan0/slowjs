file(GLOB_RECURSE SOURCES "${CMAKE_CURRENT_SOURCE_DIR}/*.c")
add_library(quickjs ${SOURCES})
target_include_directories(quickjs PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}")

target_link_libraries(quickjs libs)

option(CONFIG_BIGNUM "Enable bignumber support" ON)
if(CONFIG_BIGNUM)
  target_compile_definitions(quickjs PUBLIC CONFIG_BIGNUM=y)
else()
  target_compile_options(quickjs PUBLIC -UCONFIG_BIGNUM)
endif()

option(CONFIG_LTO "Enable Link-Time Optimization" ON)
if(CONFIG_LTO)
  # `-flto` should apply on `quickjs` directly, it's not work if `-flto` only
  # applies on the final executable `qjs`
  target_compile_options(quickjs PUBLIC $<$<CONFIG:Release>:-flto>)

  # force the `LTO` to keep the unused functions for exporting them to c modules
  target_link_options(quickjs PUBLIC $<$<CONFIG:Release>:-rdynamic>)
endif()

target_compile_options(
  quickjs
  PRIVATE -g
          -Wall
          -Wextra
          -Wno-sign-compare
          -Wno-missing-field-initializers
          -Wundef
          -Wuninitialized
          -Wunused
          -Wno-unused-parameter
          -Wwrite-strings
          -Wchar-subscripts
          -funsigned-char
          $<$<CONFIG:Debug>:-O0>
          $<$<CONFIG:Release>:-O2>)

target_link_options(quickjs PUBLIC -lm -ldl -lpthread)

if(LINUX)
  # use `lld` to work with clang on Linux
  target_link_options(quickjs PUBLIC -fuse-ld=lld)
endif()

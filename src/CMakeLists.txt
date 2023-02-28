file(GLOB_RECURSE SOURCES "${CMAKE_CURRENT_SOURCE_DIR}/*.c")
add_library(quickjs ${SOURCES})
target_include_directories(quickjs PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}")

target_link_libraries(quickjs libs)

option(QJS_CONFIG_BIGNUM "Enable bignumber support" ON)
if(QJS_CONFIG_BIGNUM)
  target_compile_definitions(quickjs PUBLIC CONFIG_BIGNUM=y)
else()
  target_compile_options(quickjs PUBLIC -UCONFIG_BIGNUM)
endif()

option(QJS_CONFIG_LTO "Enable Link-Time Optimization in release build" ON)
if(QJS_CONFIG_LTO)
  # `-flto` should apply on `quickjs` directly, it's not work if `-flto` only
  # applies on the final executable `qjs`
  target_compile_options(quickjs PUBLIC $<$<CONFIG:Release>:-flto>)
endif()

# force the `LTO` to keep the unused functions for exporting them to c modules
target_link_options(quickjs PUBLIC -rdynamic)

option(QJS_DUMP_TOKEN "Dump token while parsing" OFF)
if(QJS_DUMP_TOKEN)
  target_compile_definitions(quickjs PUBLIC DUMP_TOKEN=y)
endif()

set(QJS_DUMP_BYTECODE_HELP
    "Dump the bytecode of the compiled functions: combination of bits
// 1: dump pass 3 final byte code
// 2: dump pass 2 code
// 4: dump pass 1 code
// 8: dump stdlib functions
// 16: dump bytecode in hex
// 32: dump line number table")
option(QJS_DUMP_BYTECODE ${QJS_DUMP_BYTECODE_HELP} OFF)
if(QJS_DUMP_BYTECODE)
  target_compile_definitions(quickjs PUBLIC DUMP_BYTECODE=${QJS_DUMP_BYTECODE})
endif()

if(CMAKE_BUILD_TYPE STREQUAL "Debug")
  target_compile_definitions(quickjs PRIVATE DUMP_LEAKS=1)
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

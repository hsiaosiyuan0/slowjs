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

target_compile_options(
  quickjs
  PUBLIC -g
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

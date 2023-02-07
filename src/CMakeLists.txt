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

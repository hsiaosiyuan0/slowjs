file(GLOB_RECURSE SOURCES "${CMAKE_CURRENT_SOURCE_DIR}/*.c")
add_library(quickjs ${SOURCES})
target_include_directories(quickjs PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}")

target_link_libraries(quickjs libs)

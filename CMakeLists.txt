cmake_minimum_required(VERSION 3.14)
project(quickjs)

set(C_STANDARD C17)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

include_directories("${PROJECT_SOURCE_DIR}")

add_subdirectory(src)
add_subdirectory(libs)
add_subdirectory(qjsc)
# add_subdirectory(qjs)

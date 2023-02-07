cmake_minimum_required(VERSION 3.14)
project(quickjs)

set(C_STANDARD C17)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

include_directories("${PROJECT_SOURCE_DIR}")

# quickjs library
add_subdirectory(src)
add_subdirectory(libs)

# `qjsc` compiles js into c source file `qjs` is the default command-line
# interface shipped by quickjs
add_subdirectory(qjsc EXCLUDE_FROM_ALL)
add_subdirectory(qjs EXCLUDE_FROM_ALL)

add_subdirectory(tests EXCLUDE_FROM_ALL)
add_subdirectory(examples EXCLUDE_FROM_ALL)

add_subdirectory(unicode EXCLUDE_FROM_ALL)

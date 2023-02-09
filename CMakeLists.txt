cmake_minimum_required(VERSION 3.24)
project(quickjs)

set(C_STANDARD C17)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

include_directories("${PROJECT_SOURCE_DIR}")

if(NOT CMAKE_BUILD_TYPE OR CMAKE_BUILD_TYPE STREQUAL "")
  set(CMAKE_BUILD_TYPE
      "Debug"
      CACHE STRING "" FORCE)
endif()

message(STATUS "Build type: ${CMAKE_BUILD_TYPE}")

# quickjs library
add_subdirectory(src)
add_subdirectory(libs)

# `qjsc` compiles js into c source file, `qjs` is the command-line interface
# shipped by quickjs
add_subdirectory(qjsc EXCLUDE_FROM_ALL)
add_subdirectory(qjs EXCLUDE_FROM_ALL)

add_subdirectory(tests EXCLUDE_FROM_ALL)
add_subdirectory(examples EXCLUDE_FROM_ALL)

add_subdirectory(unicode EXCLUDE_FROM_ALL)

add_executable(test-http http.cpp)
set_property(TARGET test-http PROPERTY CXX_STANDARD 20)

find_package(Boost REQUIRED)
target_link_libraries(test-http PRIVATE Boost::boost)

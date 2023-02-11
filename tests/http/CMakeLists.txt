add_executable(testhttp http.cpp)
set_property(TARGET testhttp PROPERTY CXX_STANDARD 20)

find_package(Boost REQUIRED)
target_link_libraries(testhttp PRIVATE Boost::boost)

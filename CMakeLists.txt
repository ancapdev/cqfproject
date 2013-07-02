cmake_minimum_required(VERSION 2.8)

project(cqf)

add_subdirectory(thirdparty/nlopt-2.3)

include_directories(thirdparty)
include_directories(thirdparty/nlopt-2.3/api)

add_subdirectory(uvol)
add_subdirectory(coint)
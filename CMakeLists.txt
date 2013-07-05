cmake_minimum_required(VERSION 2.8)

project(cqf)

if(WIN32)
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /arch:SSE2 /fp:fast /GS-")
endif()

add_subdirectory(thirdparty/nlopt-2.3)

include_directories(thirdparty)
include_directories(thirdparty/nlopt-2.3/api)

if(WIN32)
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS}  /Qvec-report:2")
endif()

add_subdirectory(uvol)
add_subdirectory(coint)
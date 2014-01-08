cmake_minimum_required(VERSION 2.8)

project(cqf)

if(WIN32)
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /fp:fast /GS- /MP")
  # set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /arch:AVX")
else()
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11 -mtune=native -march=native")
  set(CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS} -O3")
endif()

add_subdirectory(thirdparty/nlopt-2.3)

include_directories(thirdparty)
include_directories(thirdparty/nlopt-2.3/api)

if(WIN32)
# set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS}  /Qvec-report:2")
endif()

add_subdirectory(uvol)
add_subdirectory(coint)
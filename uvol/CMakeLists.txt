add_executable(uvol
  fd.cpp
  fd.hpp
  main.cpp)
  
  target_link_libraries(uvol nlopt)
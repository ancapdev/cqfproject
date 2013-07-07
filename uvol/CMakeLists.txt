add_executable(uvol
  blackScholes.hpp
  finiteDifferencePricer.cpp
  finiteDifferencePricer.hpp
  main.cpp
  stopwatch.hpp
  types.hpp)
  
target_link_libraries(uvol nlopt)
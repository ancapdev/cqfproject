add_executable(uvol
  blackScholes.hpp
  finiteDifferencePricer.hpp
  main.cpp
  stopwatch.hpp
  types.hpp)
  
target_link_libraries(uvol nlopt)

add_custom_target(run-uvol
  COMMAND uvol)
  
add_executable(uvol
  avx.hpp
  blackScholes.hpp
  finiteDifferencePricer.hpp
  main.cpp
  optionContract.hpp
  stopwatch.hpp
  types.hpp)
  
target_link_libraries(uvol nlopt)

add_custom_target(run-uvol
  COMMAND uvol)
  
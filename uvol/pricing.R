# Options
options(uvol.steps1 = 170L)
options(uvol.steps2 = 200L)


# Compile C++ pricing module
library(Rcpp)
cxxflags <- paste0("-std=c++0x -Doverride= -I", getwd())
Sys.setenv("PKG_CXXFLAGS"=cxxflags)
sourceCpp("Rinterface.cpp") #, verbose=T, rebuild=T)
rm(cxxflags)

# Price a european option using black scholes
PriceEuropeanBS <- function(scenario, options) {
  CppPriceEuropeanBS(options, scenario$impliedVol, scenario$riskFreeRate, scenario$underlyingPrice)
}

# Price a european option using finite difference, allowing for uncertain volatility
PriceEuropeanUncertain <- function(scenario, options, side, steps1 = getOption('uvol.steps1'), steps2 = getOption('uvol.steps2'), interpolation = c("cubic", "linear"), detail = 0) {
  interpolation <- match.arg(interpolation)
  
  # TODO: could scale by time horizing and volatility
  maxPrice <- scenario$underlyingPrice * 2

  ds1sq <- (1 / steps1)^2
  ds2sq <- (1 / steps2)^2
  
  # Prices using a given number of asset steps
  helper <- function(steps) {
    CppPriceEuropeanUncertainVol(
      options,
      scenario$minVol,
      scenario$maxVol,
      scenario$riskFreeRate,
      scenario$underlyingPrice,
      side,
      steps,
      maxPrice,
      interpolation,
      detail)$value
  }
  
  return((helper(steps1) * ds2sq - helper(steps2) * ds1sq) / (ds2sq - ds1sq))
}



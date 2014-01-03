library(Rcpp)
library(RQuantLib)

cxxflags <- paste0("-std=c++0x -Doverride= -I", getwd())
Sys.setenv("PKG_CXXFLAGS"=cxxflags)
sourceCpp("Rinterface.cpp") #, verbose=T, rebuild=T)
rm(cxxflags)

PriceEuropeanBS <- function(scenario, type, strike, expiry) {
  EuropeanOption(
    type,
    scenario$underlyingPrice,
    strike,
    0.0,
    scenario$riskFreeRate,
    expiry,
    scenario$impliedVol)$value
}

PriceEuropeanUncertain <- function(scenario, options, side, steps1 = 50, steps2 = 70, interpolation = "cubic") {
  # TODO: could scale by time horizing and volatility
  maxPrice <- scenario$underlyingPrice * 2

  ds1sq <- (maxPrice / steps1)^2
  ds2sq <- (maxPrice / steps2)^2
  
  # Prices using a given number of asset steps
  helper <- function (steps) {
    CppPriceEuropeanUncertainVol(
      options,
      scenario$minVol,
      scenario$maxVol,
      scenario$riskFreeRate,
      scenario$underlyingPrice,
      side,
      steps,
      maxPrice,
      interpolation)$value
  }
  
  return((helper(steps1) * ds2sq - helper(steps2) * ds1sq) / (ds2sq - ds1sq))
}

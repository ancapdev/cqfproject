library(Rcpp)
# library(RQuantLib)

cxxflags <- paste0("-std=c++0x -Doverride= -I", getwd())
Sys.setenv("PKG_CXXFLAGS"=cxxflags)
sourceCpp("Rinterface.cpp") #, verbose=T, rebuild=T)
rm(cxxflags)

PriceEuropeanBS <- function(scenario, options) {
  CppPriceEuropeanBS(options, scenario$impliedVol, scenario$riskFreeRate, scenario$underlyingPrice)
}

PriceEuropeanUncertain <- function(scenario, options, side, steps1 = 100L, steps2 = 200L, interpolation = "cubic") {
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
      interpolation)$value
  }
  
  return((helper(steps1) * ds2sq - helper(steps2) * ds1sq) / (ds2sq - ds1sq))
}

PriceEuropeanUncertain2 <- function(scenario, options, side, steps = 100L, interpolation = "cubic") {
  # TODO: could scale by time horizing and volatility
  maxPrice <- scenario$underlyingPrice * 2
  
  # Prices using a given number of asset steps
  helper <- function(s) {
    CppPriceEuropeanUncertainVol(
      options,
      scenario$minVol,
      scenario$maxVol,
      scenario$riskFreeRate,
      scenario$underlyingPrice,
      side,
      s,
      maxPrice,
      interpolation)$value
  }
  
  return((4 * helper(steps * 2) - helper(steps)) / 3)
}
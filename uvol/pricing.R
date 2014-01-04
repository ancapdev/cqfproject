library(Rcpp)
library(RQuantLib)

cxxflags <- paste0("-std=c++0x -Doverride= -I", getwd())
Sys.setenv("PKG_CXXFLAGS"=cxxflags)
sourceCpp("Rinterface.cpp") #, verbose=T, rebuild=T)
rm(cxxflags)

PriceSingleEuropeanBS <- function(scenario, type, strike, expiry) {
  S <- scenario$underlyingPrice
  K <- strike
  r <- scenario$riskFreeRate
  vol <- scenario$impliedVol
  T <- expiry
  
  d1 <- (log(S / K) + (r + vol^2 / 2) * expiry) / (vol * sqrt(T))
  d2 <- d1 - vol * sqrt(T)
  
  switch(
    type,
    call = pnorm(d1) * S - pnorm(d2) * K * exp(-r * T),
    put = -pnorm(-d1) * S + pnorm(-d2) * K * exp(-r * T),
    bcall = exp(-r * T) * pnorm(d2),
    bput = exp(-r * T) * pnorm(-d2))
}


PriceEuropeanBS <- function(scenario, options) {
  sum(
    mapply(
      function(type, strike, expiry) PriceSingleEuropeanBS(scenario, type, strike, expiry),
      options$type, options$strike, options$expiry,
      USE.NAMES = FALSE) * options$qty)
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

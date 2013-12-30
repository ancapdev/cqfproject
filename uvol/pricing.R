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

PriceEuropeanUncertain <- function(scenario, options) {
  PriceOptions(
    scenario$minVol,
    scenario$maxVol,
    scenario$riskFreeRate,
    scenario$underlyingPrice,
    options)
}

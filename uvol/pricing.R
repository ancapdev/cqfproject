library(lattice)

# Options
options(uvol.steps1 = 210L)
options(uvol.steps2 = 290L)


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
PriceEuropeanUncertain <- function(
  scenario,
  options,
  side,
  steps,
  interpolation = c("cubic", "linear"),
  payoffSampling = c("interval", "point"),
  detail = 0) {

  # Verify args
  interpolation <- match.arg(interpolation)
  payoffSampling <- match.arg(payoffSampling)
  
  
  # TODO: could scale by time horizing and volatility
  maxPrice <- scenario$underlyingPrice * 2
  
  
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
    payoffSampling,
    detail)
}

# Price a european option using finite difference with Richardson extrapolation, allowing for uncertain volatility
PriceEuropeanUncertainRichardson <- function(scenario, options, side, steps1 = getOption('uvol.steps1'), steps2 = getOption('uvol.steps2'), ...) {
  # Prices using a given number of asset steps
  helper <- function(steps) PriceEuropeanUncertain(scenario, options, side, steps, ...)$value

  # Extrapolation coefficients
  ds1sq <- (1 / steps1)^2
  ds2sq <- (1 / steps2)^2
  
  return((helper(steps1) * ds2sq -  helper(steps2) * ds1sq) / (ds2sq - ds1sq))
}


# Produce 3D wireframe of value across finite difference grid used in pricing
ChartPricing <- function(scenario, options, side, steps, chartRes = 25, zrot = 40) {
  r <- PriceEuropeanUncertain(scenario, options, side, steps, detail = 2)
  
  g <- matrix(r$values, nrow = steps + 1)
  g2 <- g[,seq(1, ncol(g), length.out = chartRes)]
  g2 <- g2[seq(1, nrow(g2), length.out = chartRes),]

  wireframe(
    g2,
    row.values = r$prices[seq(1, length(r$prices), length.out = nrow(g2))],
    column.values = seq(max(options$expiry), 0, length.out = ncol(g2)),
    xlab = "Price",
    ylab = "Time",
    zlab = "Value",
    screen = list(z=zrot, x=-70, y=0),
    par.settings = list(
      box.3d = list(col=c(1, if (zrot < 0) 0 else 1, 0, if (zrot < 0) 1 else 0, 1, 0, 1, 1, 1)),
      axis.line = list(col = "transparent")),
    scales = list(
      arrows = FALSE,
      col = 1),
    col.regions = colorRampPalette(c("ivory", "lightsteelblue1", "lightsteelblue4"))(1000),
    colorkey = FALSE,
    drape = TRUE,
    pretty = TRUE)
}
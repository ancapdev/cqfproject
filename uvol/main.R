library(Rcpp)
library(RQuantLib)
library(plyr)
library(nloptr)

cxxflags <- paste0("-std=c++0x -Doverride= -I", getwd())
Sys.setenv("PKG_CXXFLAGS"=cxxflags)
sourceCpp("Rinterface.cpp") #, verbose=T, rebuild=T)
# benchmark(PriceOptions(0.1, 0.3, 0.05, 100.0, options))

CreateBinaryCall <- function(expiry, strike) {
  data.frame(
    type = "bcall",
    expiry = expiry,
    qty = 1.0,
    strike = strike,
    stringsAsFactors = FALSE)
}

exotic <- CreateBinaryCall(1.0, 100.0)

scenario <-
  list(
    minVol = 0.1,
    maxVol = 0.3,
    impliedVol = 0.2,
    underlyingPrice = 100.0,
    riskFreeRate = 0.05)




Verify <- function(scenario, exotic, quantities, strikes) {
  hedges <- ConstructHedges(exotic, quantities, strikes)
  options <- rbind(exotic, hedges)
  hedgeCost <- CalculateHedgeCost(scenario, hedges)    
  portfolioValue <- PriceEuropeanUncertain(scenario, options)
  portfolioSpread <- portfolioValue[2] - portfolioValue[1]
  exoticValue <- PriceEuropeanUncertain(scenario, exotic)
  exoticSpread <- exoticValue[2] - exoticValue[1]
  cat("Quantities: ", quantities, "\n")
  cat("Strikes:    ", strikes, "\n")
  cat("Exotic Value: ", exoticValue, "\n")
  cat("Portfolio Value: ", portfolioValue, "\n")
  cat("Hedge Cost: ", hedgeCost, "\n")
  cat("Hedged Exotic Value: ", portfolioValue - hedgeCost, "\n")
  cat("Portfolio Spread: ", portfolioSpread, "\n")
  cat("Exotic Spread:", exoticSpread, "\n")
  cat("Spread Improvement:", (exoticSpread - portfolioSpread) / exoticSpread, "\n")
}

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

ConstructHedges <- function(exotic, quantities, strikes) {
  t = if (exotic$type == "bcall") "call" else "put"
  
  hedges <- data.frame(
    type = c(t, t),
    expiry = c(exotic$expiry, exotic$expiry),
    qty = quantities,
    strike = strikes,
    stringsAsFactors = FALSE);
}

CalculateHedgeCost <- function(scenario, hedges) {
  sum(
    mapply(
      function(type, strike, expiry) PriceEuropeanBS(scenario, type, strike, expiry),
      hedges$type, hedges$strike, hedges$expiry,
      USE.NAMES = FALSE) * hedges$qty)
}

CreateObjectiveFunction <- function(scenario, exotic, side) {
  multiplier = if (side == "bid") -1.0 else 1.0
  
  function(q) {
    hedges <- ConstructHedges(exotic, q[1:2], q[3:4])
    options <- rbind(exotic, hedges)

    hedgeCost <- CalculateHedgeCost(scenario, hedges)    
    portfolioValue <- PriceEuropeanUncertain(scenario, options)
    
    return(multiplier * (portfolioValue[side] - hedgeCost))
  }  
}

CalculateLowerBounds <- function(exotic) {
  c(-2.0, 0.0, exotic$strike * 0.9, exotic$strike * 0.9)
}

CalculateUpperBounds <- function(exotic) {
  c(0.0, 2.0, exotic$strike * 1.1, exotic$strike * 1.1)
}

CalculateInitialGuess <- function(exotic) {
  c(-1.0, 1.0, exotic$strike * 0.95, exotic$strike * 1.05)
}

OptimizeHedge <- function(scenario, exotic, side) {
  # Doesn't find optimial solution starting at 100, 100
  # Much better at 95, 100
  # TOOD: use different optimization algorithm!
  # TODO: must have constraints on strike ranges
  # TODO: should bound search space by constraining quantities to [-1, 1]
  nlm(
    CreateObjectiveFunction(scenario, exotic, side),
    CalculateInitialGuess(exotic),
    iterlim=200)
}

OptimizeHedge(scenario, exotic, "bid")
OptimizeHedge(scenario, exotic, "ask")

OptimizeHedge2 <- function(scenario, exotic, side) {
  optim(
    CalculateInitialGuess(exotic),
    CreateObjectiveFunction(scenario, exotic, side),
    method = "L-BFGS-B",
    lower = CalculateLowerBounds(exotic),
    upper = CalculateUpperBounds(exotic))
}

OptimizeHedge2(scenario, exotic, "bid")
OptimizeHedge2(scenario, exotic, "ask")

OptimizeHedge3 <- function(scenario, exotic, side) {  
  nloptr(
    CalculateInitialGuess(exotic),
    CreateObjectiveFunction(scenario, exotic, side),
    lb = CalculateLowerBounds(exotic),
    ub = CalculateUpperBounds(exotic),
    opts = list(
      algorithm="NLOPT_GN_DIRECT_L",
      maxeval=10000))
}

bidOpt <- OptimizeHedge3(scenario, exotic, "bid")
askOpt <- OptimizeHedge3(scenario, exotic, "ask")

Verify(scenario, exotic, bidOpt$solution[1:2], bidOpt$solution[3:4])
Verify(scenario, exotic, askOpt$solution[1:2], askOpt$solution[3:4])


# Optimize spread rather than max(bid) + min(ask) separately
OptimizeHedge4 <- function(scenario, exotic) {  
  nloptr(
    CalculateInitialGuess(exotic),
    function(q) {
      hedges <- ConstructHedges(exotic, q[1:2], q[3:4])
      options <- rbind(exotic, hedges)
      portfolioValue <- PriceEuropeanUncertain(scenario, options)
      return(portfolioValue[2] - portfolioValue[1])
    },
    lb = CalculateLowerBounds(exotic),
    ub = CalculateUpperBounds(exotic),
    opts = list(
      algorithm="NLOPT_GN_DIRECT_L",
      maxeval=2000))
}



exotic <- CreateBinaryCall(1.0, 50.0)
opt <- OptimizeHedge4(scenario, exotic)
Verify(scenario, exotic, opt$solution[1:2], opt$solution[3:4])


overhedgeStrike = 90.0


value <- function(
  overhedgeStrike,
  hedgeQty1 = -1.0 / (strike - overhedgeStrike),
  hedgeQty2 = 1.0 / (strike - overhedgeStrike)) {
  portfolio <- PriceHedgedBinary(minVol, maxVol, riskFreeRate, underlyingPrice, expiry, strike, overhedgeStrike, hedgeQty1, hedgeQty2)
  hedgeLeg1 <- EuropeanOption("call", underlyingPrice, overhedgeStrike, 0.0, riskFreeRate, expiry, impliedVol)
  hedgeLeg2 <- EuropeanOption("call", underlyingPrice, strike, 0.0, riskFreeRate, expiry, impliedVol)
  hedgeCost <- hedgeLeg1$value * hedgeQty1 + hedgeLeg2$value * hedgeQty2
  bid <- portfolio["bid"] + hedgeCost
  ask <- portfolio["ask"] + hedgeCost
  c(bid, ask)
}


values <- sapply(seq(0, 0.2, 0.01), function(x) value(95.0, -x, x))
spreads <- values[2,] - values[1,]
plot(spreads)

bids <- matrix(NA, 10, 10)
# asks <- matrix(NA, 10, 10)
spreads <- matrix(NA, 10, 10)

rownames(spreads) <- seq(-0.4, 0.5, 0.1)
colnames(spreads) <- seq(-0.4, 0.5, 0.1)

for (i in 1:10) {
  for (j in 1:10) {
    v <- value(95.0, i / 10 - 0.5, j / 10 - 0.5)
    spreads[i, j] <- v["ask"] - v["bid"]
    # bids[i, j] <- v["bid"]
    # asks[i, j] <- v["ask"]
  }
}  

image(spreads)
spreads



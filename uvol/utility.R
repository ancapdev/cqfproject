
CreateScenario <- function(
  minVol,
  maxVol,
  impliedVol = (maxVol + minVol) / 2,
  underlyingPrice = 100.0,
  riskFreeRate = 0.05)
{
  obj <- list(
    minVol = minVol,
    maxVol = maxVol,
    impliedVol = impliedVol,
    underlyingPrice = underlyingPrice,
    riskFreeRate = riskFreeRate)
  
  class(obj) <- "scenario"
  
  obj
}

CreateOption <- function(expiry, strike, type) {
  data.frame(
    type = type,
    expiry = expiry,
    qty = 1.0,
    strike = strike,
    stringsAsFactors = FALSE)
}

CreateCall <- function(expiry, strike) CreateOption(expiry, strike, "call")
CreatePut <- function(expiry, strike) CreateOption(expiry, strike, "put")
CreateBinaryCall <- function(expiry, strike) CreateOption(expiry, strike, "bcall")
CreateBinaryPut <- function(expiry, strike) CreateOption(expiry, strike, "bput")

ConstructHedges <- function(exotic, quantities, strikes) {
  c <- length(quantities)
  
  hedges <- data.frame(
    type = rep(substr(exotic$type, 2, 5), c),
    expiry = rep(exotic$expiry, c),
    qty = quantities,
    strike = strikes,
    stringsAsFactors = FALSE);
}

# Portfolio must have exotic in row 1 and hedges in the remaining rows
CalculateHedgedPrice <- function(scenario, portfolio, side, hedgeQuantities) {
  portfolio[2:nrow(portfolio), 3] <- hedgeQuantities
  hedgeCost <- PriceEuropeanBS(scenario, portfolio[2:nrow(portfolio),])
  portfolioValue <- PriceEuropeanUncertain(scenario, portfolio, side)
  return(portfolioValue - hedgeCost)
}

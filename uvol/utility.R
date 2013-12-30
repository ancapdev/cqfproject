
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

CreateBinaryCall <- function(expiry, strike) {
  data.frame(
    type = "bcall",
    expiry = expiry,
    qty = 1.0,
    strike = strike,
    stringsAsFactors = FALSE)
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

CalculateHedgedPrice <- function(scenario, exotic, hedgeQuantities, hedgeStrikes) {
  hedges <- ConstructHedges(exotic, hedgeQuantities, hedgeStrikes)
  options <- rbind(exotic, hedges)
  hedgeCost <- CalculateHedgeCost(scenario, hedges)
  portfolioValue <- PriceEuropeanUncertain(scenario, options)
  return(portfolioValue - hedgeCost)
}

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

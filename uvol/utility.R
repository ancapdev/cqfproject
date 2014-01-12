
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

CreateOption <- function(expiry, strike, type, qty = 1) {
  data.frame(
    type = type,
    expiry = expiry,
    qty = qty,
    strike = strike,
    stringsAsFactors = FALSE)
}

CreateCall <- function(expiry, strike, ...) CreateOption(expiry, strike, "call", ...)
CreatePut <- function(expiry, strike, ...) CreateOption(expiry, strike, "put", ...)
CreateBinaryCall <- function(expiry, strike, ...) CreateOption(expiry, strike, "bcall", ...)
CreateBinaryPut <- function(expiry, strike, ...) CreateOption(expiry, strike, "bput", ...)

GetHedgeType <- function(exoticType) {
  switch(exoticType, bcall = "call", bput = "put")
}

ConstructHedges <- function(exotic, quantities, strikes) {
  c <- length(quantities)
  
  hedges <- data.frame(
    type = rep(GetHedgeType(exotic$type), c),
    expiry = rep(exotic$expiry, c),
    qty = quantities,
    strike = strikes,
    stringsAsFactors = FALSE);
}

CreateHedgedPricer <- function(scenario, exotic, side, hedgeStrikes) {
  # Portfolio of exotic and hedges
  portfolio <- rbind(
    exotic,
    ConstructHedges(exotic, rep(1, length(hedgeStrikes)), hedgeStrikes))
  
  # Values for hedge options (qty = 1)
  hedgeValues <- sapply(
    seq_along(hedgeStrikes),
    function(i) PriceEuropeanBS(scenario, portfolio[i+1,]))
  
  function(hedgeQuantities) {
    # Update hedge quantities
    portfolio$qty[2:nrow(portfolio)] <- hedgeQuantities
    # Price portfolio
    portfolioValue <- PriceEuropeanUncertainRichardson(scenario, portfolio, side)
    # Price market hedge cost
    hedgeCost <- sum(hedgeValues * hedgeQuantities)
    # Back out exotic value
    return(portfolioValue - hedgeCost)
  }
}

# Save a trellis plot
SaveTrellis <- function(fileName, plot, fileType = "pdf") {
  trellis.device(device = fileType, file = fileName)
  print(plot)
  dev.off()
}


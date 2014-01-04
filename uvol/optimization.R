library(nloptr)


# OptimizationResult constructor
OptimizationResult <- function(quantities, value, raw) {
  r <- list(quantities = quantities, value = value, raw = raw)
  class(r) <- "OptimizationResult"
  return(r)
}

# OptimzationResult custom print
print.OptimizationResult <- function(x, ...) {
  cat("Quantities: ", x$quantities, "\n")
  cat("Value: ", x$value, "\n")
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

OptimizeHedge3 <- function(scenario, exotic, side, hedgeStrikes, algorithm = "NLOPT_LN_BOBYQA") {
  # Maximize bid by minizing -bid
  scale = switch(side, bid = -1, ask = 1)
  
  result <- nloptr(
    rep(0, length(hedgeStrikes)),
    function(q) {
      hedges <- ConstructHedges(exotic, q, hedgeStrikes)
      options <- rbind(exotic, hedges)
      hedgeCost <- PriceEuropeanBS(scenario, hedges)
      portfolioValue <- PriceEuropeanUncertain(scenario, options, side)
      exoticValue = portfolioValue - hedgeCost
      return(exoticValue * scale)
    },
    lb = rep(-1, length(hedgeStrikes)),
    ub = rep(1, length(hedgeStrikes)),
    opts = list(
      # algorithm="NLOPT_GN_DIRECT_L",
      algorithm=algorithm,
      maxeval=2000))
  
  return(OptimizationResult(result$solution, result$objective * scale, result))
}

OptimizeHedge4 <- function(scenario, exotic, side, hedgeStrikes) {
  result <- optim(
    rep(0, length(hedgeStrikes)),
    function(q) {
      hedges <- ConstructHedges(exotic, q, hedgeStrikes)
      options <- rbind(exotic, hedges)
      hedgeCost <- PriceEuropeanBS(scenario, hedges)
      portfolioValue <- PriceEuropeanUncertain(scenario, options, side)
      exoticValue = portfolioValue - hedgeCost
      return(exoticValue)
    },
    lower = rep(-1, length(hedgeStrikes)),
    upper = rep(1, length(hedgeStrikes)),
    method = "L-BFGS-B",
    control = list(
      fnscale = switch(side, bid = -1, ask = 1),
      maxit = 200))
  
  return(OptimizationResult(result$par, result$value, result))
}

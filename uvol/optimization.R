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

OptimizeHedgeNlopt <- function(scenario, exotic, side, hedgeStrikes, algorithm = "NLOPT_LN_BOBYQA") {
  # Maximize bid by minizing -bid
  scale = switch(side, bid = -1, ask = 1)

  # Portfolio of exotic and hedges
  portfolio <- rbind(
    exotic,
    ConstructHedges(exotic, rep(1, length(hedgeStrikes)), hedgeStrikes))

  # Values for hedge options (qty = 1)
  hedgeValues <- sapply(
    seq_along(hedgeStrikes),
    function(i) PriceEuropeanBS(scenario, portfolio[i+1,]))
  
  result <- nloptr(
    rep(0, length(hedgeStrikes)),
    function(q) {
      # Update hedge quantities
      portfolio$qty[2:nrow(portfolio)] <- q
      # Price portfolio
      portfolioValue <- PriceEuropeanUncertain(scenario, portfolio, side)
      # Price market hedge cost
      hedgeCost <- sum(hedgeValues * q)
      # Back out exotic value
      exoticValue <- portfolioValue - hedgeCost
      # Scale to turn bid maximization into minimization for the optimizer
      return(exoticValue * scale)
    },
    lb = rep(-1, length(hedgeStrikes)),
    ub = rep(1, length(hedgeStrikes)),
    opts = list(
      algorithm=algorithm,
      maxeval=10000))
  
  return(OptimizationResult(result$solution, result$objective * scale, result))
}

OptimizeHedgeR <- function(scenario, exotic, side, hedgeStrikes) {
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

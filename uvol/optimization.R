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

OptimizeHedgeNlopt <- function(scenario, exotic, side, hedgeStrikes, algorithm = "NLOPT_LN_NELDERMEAD", maxit = 1000) {
  # Maximize bid by minizing -bid
  scale = switch(side, bid = -1, ask = 1)
  
  pricer <- CreateHedgedPricer(scenario, exotic, side, hedgeStrikes)
  
  result <- nloptr(
    rep(0, length(hedgeStrikes)),
    function(q) { pricer(q) * scale },
    lb = rep(-1, length(hedgeStrikes)),
    ub = rep(1, length(hedgeStrikes)),
    opts = list(
      algorithm = algorithm,
      maxeval = maxit,
      xtol_rel = 1e-8))
  
  return(OptimizationResult(result$solution, result$objective * scale, result))
}

OptimizeHedgeR <- function(scenario, exotic, side, hedgeStrikes, maxit = 200) {
  result <- optim(
    rep(0, length(hedgeStrikes)),
    CreateHedgedPricer(scenario, exotic, side, hedgeStrikes),
    lower = rep(-1, length(hedgeStrikes)),
    upper = rep(1, length(hedgeStrikes)),
    method = "L-BFGS-B",
    control = list(
      fnscale = switch(side, bid = -1, ask = 1),
      maxit = maxit))
  
  return(OptimizationResult(result$par, result$value, result))
}

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

# Optimize hedge for exotic for a given side, using one of nlopt's optimization algorithms
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

# Optimize hedge for exotic for a given side, using one of R's built in optimization algorithm
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

ChartOptimization <- function(scenario, exotic, side, hedgeStrikes, quantities) {
  res <- 25
  q <- quantities
  
  quantities <- expand.grid(
    q1 = seq(q[1] * 0.0, q[1] * 2.0, length.out = res),
    q2 = seq(q[2] * 0.0, q[2] * 2.0, length.out = res))
  
  value <- apply(
    quantities,
    1,
    function(x) CalculateHedgedPrice(scenario, exotic, side, x, hedgeStrikes))
  
  data <- cbind(quantities, value)
  
  wireframe(
    value ~ q1 * q2,
    data = data,
    screen = list(z=-30, x=-60, y=0),
    # screen = list(z = 90, x = 0, y = 0), # TOP DOWN
    scales = list(arrows = FALSE),
    drape = TRUE,
    pretty = TRUE)
}

ChartOptimization2 <- function(scenario, exotic, side, hedgeStrikes, quantities) {
  res <- 25
  baseQuantity <- quantities[1]
  baseScale <- quantities[2] / quantities[1]
  
  baseAndScale <- expand.grid(
    base = seq(baseQuantity * 0, baseQuantity * 2.0, length.out = res),
    scale = seq(baseScale * 0, baseScale * 2.0, length.out = res))
  
  value <- apply(
    baseAndScale,
    1,
    function(x) CalculateHedgedPrice(scenario, exotic, side, c(x[1], x[1] * x[2]), hedgeStrikes))
  
  data <- cbind(baseAndScale, value)
  
  wireframe(
    value ~ base * scale,
    data = data,
    screen = list(z=-30, x=-60, y=0),
    # screen = list(z = 90, x = 0, y = 0), # TOP DOWN
    scales = list(arrows = FALSE),
    drape = TRUE,
    pretty = TRUE)
}
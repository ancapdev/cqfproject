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

RunOptimization <- function(scenario, exotic, hedgeStrikes) {
  # Estimate error
  
  hedgeType <- GetHedgeType(exotic$type)

  maxRelError <- max(sapply(
    c(scenario$minVol, scenario$maxVol),
    function(vol) {
      testScenario <- scenario
      testScenario$minVol <- vol
      testScenario$maxVol <- vol
      testScenario$impliedVol <- vol
      max(sapply(
        c("bid", "ask"),
        function(side) {
          max(sapply(
            hedgeStrikes,
            function(strike) {
              o <- CreateOption(exotic$expiry, strike, hedgeType)
              fd <- PriceEuropeanUncertainRichardson(testScenario, o, side)
              bs <- PriceEuropeanBS(testScenario, o)
              return(abs(fd - bs) / bs)
            }))
        }))
    }))
  
  conservativeRelError <- maxRelError * 5
  
  # Optimize bid and ask
  opt <- list(
    bid = OptimizeHedgeNlopt(scenario, exotic, "bid", hedgeStrikes),
    ask = OptimizeHedgeNlopt(scenario, exotic, "ask", hedgeStrikes))
  
  # Calculate spread and relative spread (including conservative error estimate)
  bid <- opt$bid$value * (1 - conservativeRelError)
  ask <- opt$ask$value * (1 + conservativeRelError)

  portfolio <- list(
    bid = ConstructHedges(exotic, opt$bid$quantities, hedgeStrikes),
    ask = ConstructHedges(exotic, opt$ask$quantities, hedgeStrikes))
  
  charts <- expand.grid(
    side = c("bid", "ask"),
    steps = c(getOption('uvol.steps1'), getOption('uvol.steps2')),
    stringsAsFactors = FALSE)
  
  charts$priceChart <- mapply(
    function(side, steps) I(list(ChartPricing(scenario, portfolio[[side]], side, steps, zrot = -40))),
    charts$side, charts$steps)
  
  charts$precisePayoffChart <- mapply(
    function(side, steps) {
      # Calculate suitable price points for payoff chart
      p <- c(exotic$strike, hedgeStrikes)
      p <- sort(c(p - 0.001, p + 0.001))
      p <- c(2 * p[1] - p[3], p, 2 * p[length(p)] - p[length(p) - 2])
      
      return(I(list(ChartPayoffs(portfolio[[side]], p))))
    },
    charts$side, charts$steps)
  
#   # Generate charts
#   charts <- sapply(
#     c("bid", "ask"),
#     function(side) {
#       portfolio <- rbind(exotic, ConstructHedges(exotic, opt[[side]]$quantities, hedgeStrikes))
#       
#       # Generate price grid and payoff charts for each side and default step sizes
#       lapply(
#         c(getOption('uvol.steps1'), getOption('uvol.steps2')),
#         function(steps) {
#           valuation <- PriceEuropeanUncertain(scenario, portfolio, side, steps, detail = 1)
#           
#           # Calculate suitable price points for payoff chart
#           p <- c(exotic$strike, hedgeStrikes)
#           p <- sort(c(p - 0.001, p + 0.001))
#           p <- c(2 * p[1] - p[3], p, 2 * p[length(p)] - p[length(p) - 2])
#           
#           # Prices points for FD payoffs taken from subset of FD scheme returned prices
#           p2 <- valuation$prices[valuation$prices > p[1] & valuation$prices < p[length(p)]]
#           
#           return(data.frame(
#             steps = steps,
#             side = side,
#             priceChart = I(list(ChartPricing(scenario, portfolio, side, steps))),
#             precisePayoffChart = I(list(ChartPayoffs(portfolio, p))),
#             finiteDifferencePayoffChart = I(list(ChartAveragePayoffs(portfolio, p2)))))
#         })
#       
#       # For 2 strikes, generate price chart over optimization domain
#     })
  
  return(list(
    bid = bid,
    ask = ask,
    spread = ask - bid,
    relSpread = (ask - bid) / (0.5 * (ask + bid)),
    portfolio = portfolio,
    charts = charts))
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
library(lattice)
library(ggplot2)
library(gridExtra)
library(scales)

source("pricing.R")
source("utility.R")
source("optimization.R")
source("errorAnalysis.R")
source("functionAnalysis.R")

AnalyzeErrors()


CalculatePayoffs <- function(options, prices) {
  sapply(
    prices,
    function(price) {
      sum(
        mapply(
          function(type, strike, expiry) {
            switch(
              type,
              call = max(price - strike, 0),
              put = max(strike - price, 0),
              bcall = if(price > strike) 1 else 0,
              bput = if(price < strike) 1 else 0)
          },
          options$type, options$strike, options$expiry,
          USE.NAMES = FALSE) * options$qty)
    })
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

OptimizeBidAsk <- function(scenario, exotic, hedgeStrikes, algorithm = "NLOPT_LN_BOBYQA") {
  bidOpt <- OptimizeHedgeNlopt(scenario, exotic, "bid", hedgeStrikes, algorithm)
  askOpt <- OptimizeHedgeNlopt(scenario, exotic, "ask", hedgeStrikes, algorithm)
  return(list(bid=bidOpt, ask=askOpt))
}

ChartPayoffs <- function(exotic, hedgeQuantities, hedgeStrikes) {
  hedges <- ConstructHedges(exotic, hedgeQuantities, hedgeStrikes)
  portfolio <- rbind(exotic, hedges)
  payoffs <- CalculatePayoffs(portfolio, prices)
  ggplot(data.frame(price = prices, payoff = payoffs), aes(x = price, y = payoff)) +
    geom_line()
}

RunOptimizationScenario <- function(scenario, exotic, hedgeStrikes, algorithm = "NLOPT_LN_BOBYQA") {
  opt <- OptimizeBidAsk(scenario, exotic, hedgeStrikes, algorithm)
  bid <- opt$bid$value
  ask <- opt$ask$value
  mid <- (bid + ask) / 2
  spread <- ask - bid
  relSpread <- spread / mid
  cat("bid: ", bid, ", ask: ", ask, ", spread: ", spread, ", relspread: ", relSpread, "\n")
  print(ChartPayoffs(exotic, opt$bid$quantities, hedgeStrikes))
  return(opt)
}

prices <- seq(80, 120)
scenario <- CreateScenario(0.1, 0.3, underlyingPrice = 100.0)
exotic <- CreateBinaryCall(1.0, 100.0)

source("functionAnalysis.R")
source("utility.R")
source("pricing.R")
portfolio <- 
  rbind(
    exotic,
    ConstructHedges(exotic, rep(0, 5),  c(90, 95, 100, 105, 110)))
Rprof(line.profiling = TRUE)
diffs <- DifferencesFromLines(
  # function(q) CalculateHedgedPrice(scenario, exotic, "bid", q, c(90, 95, 100, 105, 110)),
  function(q) CalculateHedgedPrice2(scenario, portfolio, "bid", q),
  rep(-1, 5),
  rep(1, 5),
  500, 5)
Rprof(NULL)
summaryRprof(lines = "show")
hist(diffs)


source("utility.R")
source("optimization.R")
Rprof(line.profiling = TRUE)
RunOptimizationScenario(scenario, exotic, c(90, 95, 105, 110), "NLOPT_GN_DIRECT_L") # 2 adjacent
Rprof(NULL)
summaryRprof(lines = "show")



# exotic <- CreateBinaryPut(1.0, 100.0)
opt1 <- RunOptimizationScenario(scenario, exotic, c(95, 105)) # adjacent
opt2 <- RunOptimizationScenario(scenario, exotic, c(95, 100)) # adjacent left + self
opt3 <- RunOptimizationScenario(scenario, exotic, c(95, 100, 105)) # adjacent + self
opt4 <- RunOptimizationScenario(scenario, exotic, c(90, 95, 105, 110), "NLOPT_GN_DIRECT_L") # 2 adjacent
opt5 <- RunOptimizationScenario(scenario, exotic, c(90, 95, 100, 105, 110), "NLOPT_LN_BOBYQA") # 2 adjacent + self
opt6 <- RunOptimizationScenario(scenario, exotic, c(90, 95, 100, 105, 110), "NLOPT_GN_DIRECT_L") # 2 adjacent + self
opt7 <- RunOptimizationScenario(scenario, exotic, c(90, 95, 100, 105, 110), "NLOPT_GN_CRS2_LM") # 2 adjacent + self
opt8 <- RunOptimizationScenario(scenario, exotic, c(90, 95, 100, 105, 110), "NLOPT_GN_ISRES") # 2 adjacent + self
opt9 <- RunOptimizationScenario(scenario, exotic, c(90, 95, 100, 105, 110), "NLOPT_LN_SBPLX") # 2 adjacent + self
opt10 <- RunOptimizationScenario(scenario, exotic, c(95, 100, 105), "NLOPT_GN_CRS2_LM") # adjacent + self











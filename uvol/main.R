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


source("pricing.R")
source("errorAnalysis.R")
sce <- CreateScenario(0.2, 0.2)
o <- CreatePut(1, 101)
strikes <- seq(50, 150)
AnalyzeErrorsByStrike(sce, "put", 100, 200)

cfdr <- sapply(
  strikes,
  function(s) PriceEuropeanUncertain(sce, CreateOption(1.0, s, "call"), "bid", 189, 201, "cubic"))


lfdr <- sapply(
  strikes,
  function(s) PriceEuropeanUncertain(sce, CreateOption(1.0, s, "call"), "bid", 189, 201, "linear"))

bs <- sapply(
  strikes,
  function(s) PriceEuropeanBS(sce, CreateOption(1.0, s, "call")))

cfdre <- abs(cfdr - bs)
lfdre <- abs(lfdr - bs)
cimprov <- lfdre / cfdre

qplot(strikes, cimprov)


cfdr2 <- sapply(
  strikes,
  function(s) PriceEuropeanUncertain2(sce, CreateOption(1.0, s, "call"), "bid", 101, "cubic"))

plot(strikes, cfdr2 - bs)


source("pricing.R")
source("errorAnalysis.R")
steps <- seq(100L, 110L, 1L)
o2 <- CreateCall(1, 101)
e1 <- AnalyzeErrorsByStep(sce, o, steps, 20)
e2 <- AnalyzeErrorsByStep(sce, o2, steps, 20, interpolation = "cubic")
grid.arrange(e1, e2)

o2 <- CreateCall(1, 99)
r <- CppPriceEuropeanUncertainVol(o2, 0.2, 0.2, 0.05, 100, "bid", 101, 200, "cubic", 2)
m <- matrix(r$values, nrow = length(r$prices))
payoff <- m[,1]
value <- m[,ncol(m)]
qplot(r$prices[48:53], payoff[48:53])


bs1 <- PriceEuropeanBS(CreateScenario(0.2, 0.2, underlyingPrice = r$prices[50]), o2)
value[50] - bs1

bs2 <- PriceEuropeanBS(CreateScenario(0.2, 0.2, underlyingPrice = r$prices[51]), o2)
value[51] - bs2

bs3 <- PriceEuropeanBS(CreateScenario(0.2, 0.2, underlyingPrice = 100), o2)
r$value - bs3


ds <- r$prices[2] - r$prices[1]
index <- 52
v0 <- value[index - 1]
v1 <- value[index]
k <- (r$prices[index] - 100) / ds
v <- v0 * k + v1 * (1 - k)
print(v - r$value)
print(v0)
print(v1)
print(k)
print(v)
print(v - bs)







# References
#
# Convergence Remedies For Non-Smooth Payoffs in Option Pricing
# D. M. Pooley et al, 2002
# https://cs.uwaterloo.ca/~paforsyt/report.pdf
# 2.2 Averaging The Initial Conditions
# 2.3 Shifting The Mesh
#
#
# Discovering the Characteristics of Mathematical Programs via Sampling 
# John W Chinneck, 2000
# http://www.sce.carleton.ca/faculty/chinneck/MProbe/MProbePaper2.pdf
# 2.1 Function Shape





CalculatePayoffs <- function(options, prices) {
  sapply(
    prices,
    function(price) {
      sum(
        mapply(
          function(type, strike) {
            switch(
              type,
              call = max(price - strike, 0),
              put = max(strike - price, 0),
              bcall = if(price > strike) 1 else 0,
              bput = if(price < strike) 1 else 0)
          },
          options$type, options$strike,
          USE.NAMES = FALSE) * options$qty)
    })
}

CalculateAveragePayoff <- function(type, strike, price1, price2) {
  switch(
    type,
    call = 0.5 * (max(price1 - strike, 0) + max(price2 - strike, 0)),
    put = 0.5 * (max(strike - price1, 0) + max(strike - price2, 0)),
    bcall =
      # TODO: fix
      if(price1 > strike)
        1
      else if (price2 > strike)
        (price2 - strike) / (price2 - price1)
      else
        0,
    bput =
      if(price2 < strike)
        1
      else if (price1 < strike)
        (strike - price1) / (price2 - price1)
      else
        0)
}

CalculateFDPayoffs <- function(options, maxPrice, priceSteps) {
  prices <- seq(0, maxPrice, length.out = priceSteps + 1)
  halfDeltaPrice <- (prices[2] - prices[1]) / 2
  payoffs <- sapply(
    prices,
    function(price) {
      sum(
        mapply(
          function(type, strike) CalculateAveragePayoff(type, strike, price - halfDeltaPrice, price + halfDeltaPrice),
          options$type, options$strike,
          USE.NAMES = FALSE) * options$qty)
    })
  
  return(data.frame(price = prices, payoff = payoffs))
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

# TODO: should take prices (or price steps) as parameter, so chart can be aligned with FD grid
# TODO: should be able to calculate average payoff between prices in grid
ChartPayoffs <- function(exotic, hedgeQuantities, hedgeStrikes) {
  hedges <- ConstructHedges(exotic, hedgeQuantities, hedgeStrikes)
  portfolio <- rbind(exotic, hedges)
  prices <- seq(exotic$strike * 0.8, exotic$strike * 1.2, length.out = 1000)
  payoffs <- CalculatePayoffs(portfolio, prices)
  ggplot(data.frame(price = prices, payoff = payoffs), aes(x = price, y = payoff)) +
    geom_line()
}

ChartFDPayoffs <- function(portfolio, maxPrice, priceSteps, minChartPrice, maxChartPrice) {
  p <- CalculateFDPayoffs(portfolio, maxPrice, priceSteps)
  # TODO: could probably limit range in ggplot instead..
  p <- p[p$price >= minChartPrice & p$price <= maxChartPrice,]
  ggplot(p, aes(x = price, y = payoff)) +
    geom_line()
}

OptimizeBidAsk <- function(scenario, exotic, hedgeStrikes, ...) {
  bidOpt <- OptimizeHedgeNlopt(scenario, exotic, "bid", hedgeStrikes, ...)
  askOpt <- OptimizeHedgeNlopt(scenario, exotic, "ask", hedgeStrikes, ...)
  return(list(bid=bidOpt, ask=askOpt))
}

RunOptimizationScenario <- function(scenario, exotic, hedgeStrikes, ...) {
  opt <- OptimizeBidAsk(scenario, exotic, hedgeStrikes, ...)
  bid <- opt$bid$value
  ask <- opt$ask$value
  mid <- (bid + ask) / 2
  spread <- ask - bid
  relSpread <- spread / mid
  cat("bid: ", bid, ", ask: ", ask, ", spread: ", spread, ", relspread: ", relSpread, "\n")
  
  
  hedges <- ConstructHedges(exotic, opt$bid$quantities, hedgeStrikes)
  portfolio <- rbind(exotic, hedges)
  
  p0 <- ChartPayoffs(exotic, opt$bid$quantities, hedgeStrikes)
  p1 <- ChartFDPayoffs(portfolio, 200, 101, 80, 120)
  p2 <- ChartFDPayoffs(portfolio, 200, 202, 80, 120)
  grid.arrange(p0, p1, p2)
  
  return(opt)
}

prices <- seq(80, 120)
scenario <- CreateScenario(0.1, 0.3, underlyingPrice = 100.0)
exotic <- CreateBinaryCall(1.0, 100.0)

source("functionAnalysis.R")
source("utility.R")
source("pricing.R")
hedgeStrikes <- c(90, 95, 100, 105, 110)
pricer <- CreateHedgedPricer(scenario, exotic, "bid", hedgeStrikes)
Rprof(line.profiling = TRUE)
diffs <- DifferencesFromLines(
  pricer,
  rep(-1, 5),
  rep(1, 5),
  500, 5)
Rprof(NULL)
summaryRprof(lines = "show")
hist(diffs)


source("optimization.R")
opt1 <- RunOptimizationScenario(scenario, exotic, c(100))
opt2 <- RunOptimizationScenario(scenario, exotic, c(95, 100))
opt3 <- RunOptimizationScenario(scenario, exotic, c(95, 105))
opt4 <- RunOptimizationScenario(scenario, exotic, c(95, 100, 105))
opt5 <- RunOptimizationScenario(scenario, exotic, c(90, 95, 105, 110))
opt6 <- RunOptimizationScenario(scenario, exotic, c(90, 95, 100, 105, 110, maxit=10000))
opt7 <- RunOptimizationScenario(scenario, exotic, c(85, 90, 95, 100, 105, 110, 115), maxit=10000)

opt1 <- RunOptimizationScenario(scenario, exotic, c(100))
opt2 <- RunOptimizationScenario(scenario, exotic, c(99, 100))
opt3a <- RunOptimizationScenario(scenario, exotic, c(99, 101))
opt3b <- RunOptimizationScenario(scenario, exotic, c(100, 101))
opt4 <- RunOptimizationScenario(scenario, exotic, c(99, 100, 101))
opt5 <- RunOptimizationScenario(scenario, exotic, c(98, 99, 101, 102))
opt6 <- RunOptimizationScenario(scenario, exotic, c(98, 99, 100, 101, 102))
opt7 <- RunOptimizationScenario(scenario, exotic, c(97, 98, 99, 100, 101, 102, 103), maxit=10000)

print(opt$bid$raw)






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



source("pricing.R")
source("errorAnalysis.R")
steps <- seq(30L, 110L, 1L)
o2 <- CreateCall(1, 101)
e1 <- AnalyzeErrorsByStep(sce, o2, steps, 20)
e2 <- AnalyzeErrorsByStep(sce, o2, steps, 20, interpolation = "linear")
grid.arrange(e1, e2)

o2 <- CreateCall(1, 99)
r <- CppPriceEuropeanUncertainVol(o2, 0.2, 0.2, 0.05, 100, "bid", 101, 200, "cubic", 2)
m <- matrix(r$values, nrow = length(r$prices))
payoff <- m[,1]
value <- m[,ncol(m)]
qplot(r$prices[48:53], payoff[48:53])



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
    
    # TODO: verify
    call =
      if(price1 > strike)
        0.5 * (price1 + price2) - strike
      else if (price2 > strike)
        0.5 * (price2 - strike)^2 / (price2 - price1)
      else
        0,

    put =
      if(price2 < strike)
        strike - 0.5 * (price1 + price2)
      else if (price1 < strike)
        0.5 * (strike - price1)^2 / (price2 - price1)
      else
        0,

    bcall =
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

# TODO: should take prices (or price steps) as parameter, so chart can be aligned with FD grid
# TODO: should be able to calculate average payoff between prices in grid
ChartPayoffs <- function(exotic, hedgeQuantities, hedgeStrikes) {
  hedges <- ConstructHedges(exotic, hedgeQuantities, hedgeStrikes)
  portfolio <- rbind(exotic, hedges)
  prices <- seq(exotic$strike * 0.95, exotic$strike * 1.05, length.out = 1000)
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
  p1 <- ChartFDPayoffs(portfolio, 200, 170, 95, 105)
  p2 <- ChartFDPayoffs(portfolio, 200, 200, 95, 105)
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






library(lattice)
library(ggplot2)
library(gridExtra)
library(scales)

source("pricing.R")
source("utility.R")
source("optimization.R")
source("errorAnalysis.R")
source("functionAnalysis.R")
source("payoffAnalysis.R")

AnalyzeErrors()


source("payoffAnalysis.R")
o <- rbind(
  CreateCall(1, 100),
  CreateBinaryCall(1, 100.0))
prices <- seq(97, 102, 1)
c1 <- ChartPayoffs(o, prices[-1])
c2 <- ChartAveragePayoffs(o, prices + 0.5)
grid.arrange(c1, c2)


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






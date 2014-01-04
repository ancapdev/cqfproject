library(lattice)
library(ggplot2)
library(gridExtra)
library(scales)

source("pricing.R")
source("utility.R")
source("optimization.R")
source("errorAnalysis.R")


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

ChartOptimization <- function(scenario, exotic, side, hedgeStrikes, optResult) {
  res <- 25
  q <- optResult$solution
  
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
    # screen = list(z=45, y=30, z=45, x=-90, y=45),
    screen = list(z=-30, x=-60, y=0),
    drape = TRUE,
    pretty = TRUE)
}

source("optimization.R")
prices <- seq(80, 120)
scenario <- CreateScenario(0.1, 0.3, underlyingPrice = 100.0)
exotic <- CreateBinaryCall(1.0, 100.0)
hedgeStrikes <- c(95, 105)
bidOpt1 <- OptimizeHedge4(scenario, exotic, "bid", hedgeStrikes)
hedges1 <- ConstructHedges(exotic, bidOpt1$quantities, hedgeStrikes)
portfolio1 <- rbind(exotic, hedges1)
payoffs1 <- CalculatePayoffs(portfolio1, prices)
ggplot(data.frame(price = prices, payoff = payoffs1), aes(x = price, y = payoff)) +
  geom_line()

bidOpt2 <- OptimizeHedge3(scenario, exotic, "bid", hedgeStrikes)
print(bidOpt2)

strikes3 <- c(90, 95, 105, 110)
bidOpt3 <- OptimizeHedge4(scenario, exotic, "bid", strikes3)
hedges3 <- ConstructHedges(exotic, bidOpt3$quantities, strikes3)
portfolio3 <- rbind(exotic, hedges3)
payoffs3 <- CalculatePayoffs(portfolio3, prices)
ggplot(data.frame(price = prices, payoff = payoffs3), aes(x = price, y = payoff)) +
  geom_line()
print(bidOpt3)


ChartOptimization(scenario, exotic, "bid", hedgeStrikes, bidOpt)
ChartOptimization(scenario, exotic, "ask", hedgeStrikes, askOpt)











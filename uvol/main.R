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


source("pricing.R")
ChartPricing(
  CreateScenario(0.1, 0.3),
  rbind(CreateCall(1, 100), CreatePut(1, 100)),
  "ask", 100)


r <- PriceEuropeanUncertain(CreateScenario(0.1, 0.3), CreateCall(1, 100), "ask", 100, detail = 2)

res <- 25
g <- matrix(r$values, nrow = 101)
g2 <- g[,seq(1, ncol(g), length.out = res)]
g2 <- g2[seq(1, nrow(g2), length.out = res),]

wireframe(
  g2,
  row.values = seq(0, 200, length.out = nrow(g2)),
  column.values = seq(1, 0, length.out = ncol(g2)),
  xlab = "Price",
  ylab = "Time",
  zlab = "Value",
  scales = list(arrows = FALSE),
  col.regions = colorRampPalette(c("ivory", "lightsteelblue1", "lightsteelblue4"))(1000),
  drape = TRUE,
  pretty = TRUE)



wireframe(
  value ~ q1 * q2,
  data = data,
  screen = list(z=-30, x=-60, y=0),
  # screen = list(z = 90, x = 0, y = 0), # TOP DOWN
  scales = list(arrows = FALSE),
  drape = TRUE,
  pretty = TRUE)






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
# John W Chinneck, 2000fin
# http://www.sce.carleton.ca/faculty/chinneck/MProbe/MProbePaper2.pdf
# 2.1 Function Shape



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






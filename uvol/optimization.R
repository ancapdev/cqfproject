library(nloptr)
library(ggplot2)
library(scales)

# Load script dependencies
if (!exists("CreateScenario", mode = "function")) source("utility.R")
if (!exists("PriceEuropeanBS", mode = "function")) source("pricing.R")
if (!exists("ChartPayoffs", mode = "function")) source("payoffAnalysis.R")

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

# Run optimization and produce analysis
RunOptimization <- function(scenario, exotic, hedgeStrikes) {
  # Optimize bid and ask
  opt <- list(
    bid = OptimizeHedgeNlopt(scenario, exotic, "bid", hedgeStrikes),
    ask = OptimizeHedgeNlopt(scenario, exotic, "ask", hedgeStrikes))
  
  # Construct optimized portfolios
  portfolio <- list(
    bid = rbind(exotic, ConstructHedges(exotic, opt$bid$quantities, hedgeStrikes)),
    ask = rbind(exotic, ConstructHedges(exotic, opt$ask$quantities, hedgeStrikes)))
  
  # Estimate errors in portfolio pricing (based on constant volatility)
  calculateError <- function(side) {
    max(sapply(
      c(scenario$minVol, scenario$maxVol),
      function(vol) {
        testScenario <- scenario
        testScenario$minVol <- vol
        testScenario$maxVol <- vol
        testScenario$impliedVol <- vol
      
        fd <- PriceEuropeanUncertainRichardson(testScenario, portfolio[[side]], side)
        bs <- PriceEuropeanBS(testScenario, portfolio[[side]])
        return(abs((fd - bs) / bs))
      }))
  }

  # Make conservative estimate of error as multiple of calculated error
  relError = list(
    bid = calculateError("bid") * 2,
    ask = calculateError("ask") * 2)
  
  # Calculate spread and relative spread (including conservative error estimate)
  bid <- opt$bid$value
  ask <- opt$ask$value
  adjustedBid <- bid * (1 - relError$bid)
  adjustedAsk <- ask * (1 + relError$ask)

  # Charts of value across finite difference pricing grid
  pricingChart = list(
    steps1 = list(
      bid = ChartPricing(scenario, portfolio$bid, "bid", getOption('uvol.steps1'), zrot = -40),
      ask = ChartPricing(scenario, portfolio$ask, "ask", getOption('uvol.steps1'), zrot = -40)),
    steps2 = list(
      bid = ChartPricing(scenario, portfolio$bid, "bid", getOption('uvol.steps2'), zrot = -40),
      ask = ChartPricing(scenario, portfolio$ask, "ask", getOption('uvol.steps2'), zrot = -40)))

  # Charts of precise payoffs
  chartPrecise <- function(side) {
    p <- c(exotic$strike, hedgeStrikes)
    p <- sort(c(p - 0.001, p + 0.001))
    p <- c(0, p, scenario$underlyingPrice * 2)
    
    return(ChartPayoffs(portfolio[[side]], p))
  }
  
  precisePayoffChart <- list(
    bid = chartPrecise("bid"),
    ask = chartPrecise("ask"))
  
  # Charts of finite difference grid payoffs
  fdPayoffChart <- list(
    steps1 = list(
      bid = ChartAveragePayoffs(portfolio$bid, seq(0, scenario$underlyingPrice * 2, length.out = getOption('uvol.steps1') + 1)),
      ask = ChartAveragePayoffs(portfolio$ask, seq(0, scenario$underlyingPrice * 2, length.out = getOption('uvol.steps1') + 1))),
    steps2 = list(
      bid = ChartAveragePayoffs(portfolio$bid, seq(0, scenario$underlyingPrice * 2, length.out = getOption('uvol.steps2') + 1)),
      ask = ChartAveragePayoffs(portfolio$ask, seq(0, scenario$underlyingPrice * 2, length.out = getOption('uvol.steps2') + 1))))
  

  # TODO: For 2 strikes, generate price chart over optimization domain
  optimizationChart <- if (length(hedgeStrikes) == 2) {
    list(
      bid = ChartOptimization(scenario, exotic, "bid", hedgeStrikes, c(0, 0), opt$bid$quantities * 2),
      ask = ChartOptimization(scenario, exotic, "ask", hedgeStrikes, c(0, 0), opt$ask$quantities * 2))
  } else {
    NULL
  }

  return(list(
    opt = opt,
    bid = bid,
    ask = ask,
    spread = ask - bid,
    relSpread = (ask - bid) / (0.5 * (ask + bid)),
    relError = relError,
    adjustedBid = adjustedBid,
    adjustedAsk = adjustedAsk,
    adjustedSpread = adjustedAsk - adjustedBid,
    adjustedRelSpread = (adjustedAsk - adjustedBid) / (0.5 * (adjustedAsk + adjustedBid)),
    portfolio = portfolio,
    precisePayoffChart = precisePayoffChart,
    fdPayoffChart = fdPayoffChart,
    pricingChart = pricingChart,
    optimizationChart = optimizationChart))
}

# Create 3D wireframe plot of value around optimum quantities
ChartOptimization <- function(scenario, exotic, side, hedgeStrikes, minQuantities, maxQuantities, res = 25) {
  quantities <- expand.grid(
    q1 = seq(minQuantities[1], maxQuantities[1], length.out = res),
    q2 = seq(minQuantities[2], maxQuantities[2], length.out = res))

  value <- apply(quantities, 1, CreateHedgedPricer(scenario, exotic, side, hedgeStrikes))
  
  data <- cbind(quantities, value)
  
  wireframe(
    value ~ q1 * q2,
    data = data,
    xlab = paste(hedgeStrikes[1], "quantity"),
    ylab = paste(hedgeStrikes[2], "quantity"),
    zlab = "Value",
    screen = list(z=-30, x=-60, y=0),
    # screen = list(z = 90, x = 0, y = 0), # TOP DOWN
    scales = list(arrows = FALSE, col = 1),
    par.settings = list(axis.line = list(col = "transparent")),
    col.regions = colorRampPalette(c("ivory", "lightsteelblue1", "lightsteelblue4"))(1000),
    colorkey = FALSE,
    drape = TRUE,
    pretty = TRUE)
}


# Set up scenario and run optimizations over a varying set of hedge configurations
RunOptimizationExperiment <- function() {
  dir.create(file.path(getwd(), "charts"), showWarnings = FALSE)
  chartType <- "pdf"
  scenario <- CreateScenario(0.1, 0.3, underlyingPrice = 100.0, riskFreeRate = 0.05)
  exotic <- CreateBinaryCall(1, 100)
  strikes <- list(
    c(95, 105),
    c(90, 95, 105, 110),
    c(99, 101),
    c(98, 98, 101, 102))
  
  pb <- txtProgressBar(max = length(strikes) * 6 + 4)
  
  # TODO: Use utility function
  saveTrellis <- function(fileName, plot) {
    trellis.device(device = chartType, file = fileName)
    print(plot)
    dev.off()
    setTxtProgressBar(pb, getTxtProgressBar(pb) + 1)
  }
  
  myggsave <- function(fileName, plot) {
    ggsave(fileName, plot, width = 14, height = 14)
    setTxtProgressBar(pb, getTxtProgressBar(pb) + 1)
  }
  
  for (hedgeStrikes in strikes) {
    o <- RunOptimization(scenario, exotic, hedgeStrikes)
    setTxtProgressBar(pb, getTxtProgressBar(pb) + 1)
    
    # Descriptive name for output files
    id <- do.call(paste, c(hedgeStrikes, list(sep = "_")))
    
    for (side in c("bid", "ask")) {
      myggsave(paste0("charts/opt_", id, "_payoff_fd1_", side, ".", chartType),
             o$fdPayoffChart$steps1[[side]])
      
      myggsave(paste0("charts/opt_", id, "_payoff_fd2_", side, ".", chartType),
             o$fdPayoffChart$steps2[[side]])
      
      myggsave(paste0("charts/opt_", id, "_payoff_real_", side, ".", chartType),
             o$precisePayoffChart[[side]])
      
      saveTrellis(paste0("charts/opt_", id, "_price_grid1_", side, ".", chartType),
                  o$pricingChart$steps1[[side]])
      
      saveTrellis(paste0("charts/opt_", id, "_price_grid2_", side, ".", chartType),
                  o$pricingChart$steps2[[side]])
      
      if (!is.null(o$optimizationChart)) {
        saveTrellis(paste0("charts/opt_", id, "_objective_", side, ".", chartType),
                    o$optimizationChart[[side]])
      }
    }  
  }
  close(pb)

  #     o$fdPayoffChart$steps2$bid +
  #       geom_line() + geom_point() +
  #       facet_grid(option ~ ., scales = "free_y") +
  #       # coord_cartesian(xlim = c(95, 105), ylim = c(-1.5, 1.5)) +
  #       xlim(95, 105) +
  #       ylim(-1.5, 1.5) +
  #       theme(legend.position = "none")
}

source("errorAnalysis.R")
source("functionAnalysis.R")
source("optimization.R")
source("payoffAnalysis.R")
source("pricing.R")
source("utility.R")

RunExamples <- function() {
  dir.create(file.path(getwd(), "charts"), showWarnings = FALSE)
  
  myggsave <- function(fileName, plot) {
    ggsave(fileName, plot, width = 10, height = 6)
  }
  
  #
  # Pricing charts
  #
  SaveTrellis(
    "charts/example_pricing.pdf",
    ChartPricing(CreateScenario(0.2, 0.2), CreateCall(1, 100), "bid", 100))
  
  #
  # Error charts
  #
  myggsave(
    "charts/example_errors_by_step.pdf",
    AnalyzeErrorsByStep(CreateScenario(0.2, 0.2), CreateCall(1, 100)))

  myggsave(
    "charts/example_errors_by_grid_price.pdf",
    AnalyzeErrorsByGridPrice(CreateScenario(0.2, 0.2), CreateCall(1, 100)))
  
  myggsave(
    "charts/example_errors_by_strike.pdf",
    AnalyzeErrorsByStrike(CreateScenario(0.2, 0.2), "call"))
  
  #
  # Payoff charts
  #
  myggsave(
    "charts/example_payoffs.pdf",
    ChartPayoffs(CreateCall(1, 100), 98:102))
  
  myggsave(
    "charts/example_average_payoffs.pdf",
    ChartAveragePayoffs(CreateCall(1, 100), 98:102))
  
  p <- ChartPayoffs(
    rbind(
      CreateBinaryCall(1, 100),
      CreateCall(1, 95, qty = -0.1),
      CreateCall(1, 105, qty = 0.1)),
    seq(90, 110, length.out = 200)) +
    facet_grid(option ~ ., scales = "free_y") +
    theme(legend.position = "none")
  
  myggsave("charts/example_hedged_payoffs.pdf", p)
  
  #
  # Optimization charts
  #
  SaveTrellis(
    "charts/example_optimization.pdf",
    ChartOptimization(
      CreateScenario(0.1, 0.3),
      CreateBinaryCall(1, 100),
      "bid",
      c(95, 105),
      c(0, 0),
      c(-0.5, 0.5)))
  
  
  f <- CreateHedgedPricer(
    CreateScenario(0.1, 0.3),
    CreateBinaryCall(1, 100),
    "bid",
    c(90, 95, 105, 110))
  
  d <- DifferencesFromLines(f, rep(-1, 4), rep(1, 4), 100)
  myggsave(
    "charts/example_function_shape.pdf",
    qplot(d, geom = "histogram", xlab = "Differences from line", ylab = "Count"))
}

RunExamples()

PricingWithConstantVolExample <- function() {
params <- expand.grid(
vol = c(0.2, 0.3),
expiry = c(1, 2),
strike = c(90, 100, 110))

values <- do.call(
  rbind,
  mapply(
    function(vol, expiry, strike) {
      scenario <- CreateScenario(vol, vol)
      option <- CreateCall(expiry, strike)
      data.frame(
        bs = PriceEuropeanBS(scenario, option),
        fd = PriceEuropeanUncertain(scenario, option, "bid", 100)$value,
        fdr = PriceEuropeanUncertainRichardson(scenario, option, "bid", 50, 100))
    },
    params$vol, params$expiry, params$strike,
    SIMPLIFY = FALSE))

cbind(params, values)
}

PricingWithUncertainVolExample <- function() {
scenario <- CreateScenario(0.1, 0.3)
option <- CreateCall(1, 100)
PriceEuropeanUncertainRichardson(scenario, option, "bid")
PriceEuropeanUncertainRichardson(scenario, option, "ask")
PriceEuropeanBS(CreateScenario(0.1, 0.1), option)
PriceEuropeanBS(CreateScenario(0.3, 0.3), option)
}

PricingBinaryWithUncertainVolExample <- function() {
scenario1 <- CreateScenario(0.2, 0.3)
scenario2 <- CreateScenario(0.1, 0.4)
option <- CreateBinaryCall(1, 100)
PriceEuropeanUncertainRichardson(scenario1, option, "bid")
PriceEuropeanUncertainRichardson(scenario1, option, "ask")
PriceEuropeanUncertainRichardson(scenario2, option, "bid")
PriceEuropeanUncertainRichardson(scenario2, option, "ask")
PriceEuropeanBS(CreateScenario(0.2, 0.2), option)
PriceEuropeanBS(CreateScenario(0.3, 0.3), option)
}

PriceHedgedExoticExample <- function() {
scenario <- CreateScenario(0.1, 0.3)
exotic <- CreateBinaryCall(1, 100)
hedges <- rbind(
  CreateCall(1, 95, qty = -0.1),
  CreateCall(1, 105, qty = 0.1))
portfolio <- rbind(exotic, hedges)
exoticValue <- PriceEuropeanUncertainRichardson(scenario, exotic, "bid")
basketValue <- PriceEuropeanUncertainRichardson(scenario, portfolio, "bid")
hedgeCost <- PriceEuropeanBS(scenario, hedges)
hedgedExoticValue <- basketValue - hedgeCost
exoticValue
hedgedExoticValue
}

FuncionShapeExample <- function() {

}


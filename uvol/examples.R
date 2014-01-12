source("errorAnalysis.R")
source("optimization.R")
source("payoffAnalysis.R")
source("pricing.R")
source("utility.R")

RunExamples <- function() {
  dir.create(file.path(getwd(), "charts"), showWarnings = FALSE)
  
  #
  # Pricing charts
  #
  SaveTrellis(
    "charts/example_pricing.pdf",
    ChartPricing(CreateScenario(0.2, 0.2), CreateCall(1, 100), "bid", 100))
  
  #
  # Error charts
  #
  ggsave(
    "charts/example_errors_by_step.pdf",
    AnalyzeErrorsByStep(CreateScenario(0.2, 0.2), CreateCall(1, 100)))

  ggsave(
    "charts/example_errors_by_grid_price.pdf",
    AnalyzeErrorsByGridPrice(CreateScenario(0.2, 0.2), CreateCall(1, 100)))
  
  ggsave(
    "charts/example_errors_by_strike.pdf",
    AnalyzeErrorsByStrike(CreateScenario(0.2, 0.2), "call"))
  
  #
  # Payoff charts
  #
  ggsave(
    "charts/example_payoffs.pdf",
    ChartPayoffs(CreateCall(1, 100), 98:102))
  
  ggsave(
    "charts/example_average_payoffs.pdf",
    ChartAveragePayoffs(CreateCall(1, 100), 98:102))
  
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
  
}

RunExamples()
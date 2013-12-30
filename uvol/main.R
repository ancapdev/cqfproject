source("pricing.R")
source("utility.R")
source("optimize.R")

exotic <- CreateBinaryCall(1.0, 100.0)
scenario <- CreateScenario(0.1, 0.3, underlyingPrice = 100.0)

opt <- OptimizeHedge(scenario, exotic)
Verify(scenario, exotic, opt$solution[1:2], opt$solution[3:4])

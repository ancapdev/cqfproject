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

source("pricing.R")
source("errorAnalysis.R")
AnalyzeErrors()

source("optimization.R")
RunOptimizationExperiment()

source("pricing.R")
source("payoffAnalysis.R")
source("optimization.R")
o <- RunOptimization(
  CreateScenario(0.1, 0.3),
  CreateBinaryCall(1, 100),
  c(95, 105))
  # c(98, 99, 101, 102))

print(o$optimizationChart$bid)
print(str(o, max.level = 1))

o$fdPayoffChart$steps2$bid +
  geom_line() + geom_point() +
  facet_grid(option ~ ., scales = "free_y") +
  # coord_cartesian(xlim = c(95, 105), ylim = c(-1.5, 1.5)) +
  xlim(95, 105) +
  ylim(-1.5, 1.5) +
  theme(legend.position = "none")






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








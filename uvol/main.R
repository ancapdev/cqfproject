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
source("payoffAnalysis.R")
source("optimization.R")
o <- RunOptimization(
  CreateScenario(0.1, 0.3),
  CreateBinaryCall(1, 100),
  c(99, 101))
  # c(96, 98, 102, 104))








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








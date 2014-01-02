library(nloptr)

CalculateLowerBounds <- function(exotic) {
  c(-2.0, 0.0, exotic$strike * 0.9, exotic$strike * 0.9)
}

CalculateUpperBounds <- function(exotic) {
  c(0.0, 2.0, exotic$strike * 1.1, exotic$strike * 1.1)
}

CalculateInitialGuess <- function(exotic) {
  c(-1.0, 1.0, exotic$strike * 0.95, exotic$strike * 1.05)
}

# Optimize spread rather than max(bid) + min(ask) separately
OptimizeHedge <- function(scenario, exotic) {  
  nloptr(
    CalculateInitialGuess(exotic),
    function(q) {
      hedges <- ConstructHedges(exotic, q[1:2], q[3:4])
      options <- rbind(exotic, hedges)
      portfolioValue <- PriceEuropeanUncertain(scenario, options)
      return(portfolioValue[2] - portfolioValue[1])
    },
    lb = CalculateLowerBounds(exotic),
    ub = CalculateUpperBounds(exotic),
    opts = list(
      algorithm="NLOPT_GN_DIRECT_L",
      maxeval=2000))
}

# WIP
OptimizeHedge2 <- function(scenario, exotic) {  
  nloptr(
    c(0, 0),
    function(q) {
      hedges <- ConstructHedges(exotic, q[1:2], c(95, 105))
      options <- rbind(exotic, hedges)
      portfolioValue <- PriceEuropeanUncertain(scenario, options)
      return(portfolioValue[2] - portfolioValue[1])
    },
    lb = c(-20, -20),
    ub = c(20, 20),
    opts = list(
      algorithm="NLOPT_GN_DIRECT_L",
      maxeval=2000))
}


#
# Functions to optimize bid/ask separately.
#

# CreateObjectiveFunction <- function(scenario, exotic, side) {
#   multiplier = if (side == "bid") -1.0 else 1.0
#   
#   function(q) {
#     hedges <- ConstructHedges(exotic, q[1:2], q[3:4])
#     options <- rbind(exotic, hedges)
#     
#     hedgeCost <- CalculateHedgeCost(scenario, hedges)    
#     portfolioValue <- PriceEuropeanUncertain(scenario, options)
#     
#     return(multiplier * (portfolioValue[side] - hedgeCost))
#   }  
# }
# 
# 
# OptimizeHedge1 <- function(scenario, exotic, side) {
#   # Doesn't find optimial solution starting at 100, 100
#   # Much better at 95, 100
#   # TOOD: use different optimization algorithm!
#   # TODO: must have constraints on strike ranges
#   # TODO: should bound search space by constraining quantities to [-1, 1]
#   nlm(
#     CreateObjectiveFunction(scenario, exotic, side),
#     CalculateInitialGuess(exotic),
#     iterlim=200)
# }
# 
# OptimizeHedge2 <- function(scenario, exotic, side) {
#   optim(
#     CalculateInitialGuess(exotic),
#     CreateObjectiveFunction(scenario, exotic, side),
#     method = "L-BFGS-B",
#     lower = CalculateLowerBounds(exotic),
#     upper = CalculateUpperBounds(exotic))
# }
# 
# OptimizeHedge3 <- function(scenario, exotic, side) {  
#   nloptr(
#     CalculateInitialGuess(exotic),
#     CreateObjectiveFunction(scenario, exotic, side),
#     lb = CalculateLowerBounds(exotic),
#     ub = CalculateUpperBounds(exotic),
#     opts = list(
#       algorithm="NLOPT_GN_DIRECT_L",
#       maxeval=10000))
# }
library(lattice)

source("pricing.R")
source("utility.R")
source("optimize.R")


exotic <- CreateBinaryCall(1.0, 100.0)
scenario <- CreateScenario(0.1, 0.3, underlyingPrice = 100.0)

opt <- OptimizeHedge(scenario, exotic)
Verify(scenario, exotic, opt$solution[1:2], opt$solution[3:4])


CalculateHedgedSpread <- function(scenario, exotic, hedgeQuantities, hedgeStrikes) {
  hedgedPrice <- CalculateHedgedPrice(scenario, exotic, hedgeQuantities, hedgeStrikes)
  unname(hedgedPrice[2] - hedgedPrice[1])
}

CalcByQty <- Vectorize(function(x) {
  CalculateHedgedSpread(scenario, exotic, opt$solution[1:2] * x, opt$solution[3:4])
})


CalcByStrike <- Vectorize(function(x) {
  CalculateHedgedSpread(scenario, exotic, opt$solution[1:2], opt$solution[3:4] * x)
})

curve(CalcByQty, 0.8, 1.2)

curve(CalcByStrike, 0.8, 1.2)


res <- 20
q <- opt$solution[1:2]
s <- opt$solution[3:4]

#########################

quantities <- expand.grid(
  q1 = seq(q[1] * 0.8, q[1] * 1.2, length.out = res),
  q2 = seq(q[2] * 0.8, q[2] * 1.2, length.out = res))

spread <- apply(
  quantities,
  1,
  function(x) CalculateHedgedSpread(scenario, exotic, x, s))

data <- cbind(quantities, spread)

wireframe(
  spread ~ q1 * q2,
  data = data)
  
#########################

strikes <- expand.grid(
  s1 = seq(s[1] * 0.99, s[1] * 1.01, length.out = res),
  s2 = seq(s[2] * 0.99, s[2] * 1.01, length.out = res))

spread <- apply(
  strikes,
  1,
  function(x) CalculateHedgedSpread(scenario, exotic, q, x))

data <- cbind(strikes, log(spread))

wireframe(
  spread ~ s1 * s2,
  data = data,
  screen = list(z=45, y=30, z=45, x=-90, y=45),
  drape = TRUE,
  pretty = TRUE)

#########################

quantstrikes <- expand.grid(
  q1 = seq(q[1] * 0.1, q[1] * 2.0, length.out = res),
  s1 = seq(s[1] * 0.5, s[1] * 1.5, length.out = res))

spread <- apply(
  quantstrikes,
  1,
  function(x) CalculateHedgedSpread(scenario, exotic, c(x[1], q[2]), c(x[2], s[2])))

data <- cbind(quantstrikes, spread)

wireframe(
  spread ~ q1 * s1,
  data = data,
  screen = list(z=70, x=-60, y=0),
  drape = TRUE,
  pretty = TRUE)










library(lattice)
library(ggplot2)
library(gridExtra)
library(scales)

source("pricing.R")
source("utility.R")
source("optimization.R")
source("errorAnalysis.R")
AnalyzeErrors()


scenario <- CreateScenario(0.2, 0.2, underlyingPrice = 100.0)
exotic <- CreateBinaryCall(1.0, 100.0)
vanilla <- CreatePut(1.0, 80.0)

p1 <- PriceEuropeanBS(scenario, vanilla)
p2 <- PriceEuropeanUncertain(scenario, vanilla, "bid", 50, 80)
print(p1)
print(p2)
print(1 - p1 / p2)


Richardson <- function(minVol, maxVol, riskFree, price, side, steps1, steps2, interpolation, options) {
  result1 <- CppPriceEuropeanUncertainVol(options, minVol, maxVol, riskFree, price, side, steps1, price * 2, interpolation)$value
  result2 <- CppPriceEuropeanUncertainVol(options, minVol, maxVol, riskFree, price, side, steps2, price * 2, interpolation)$value
  
  ds1 <- 2 * price / steps1
  ds2 <- 2 * price / steps2
  ds1sq <- ds1 * ds1
  ds2sq <- ds2 * ds2
  
  result <- (result1 * ds2sq - result2 * ds1sq) / (ds2sq - ds1sq)

  # print(result1)
  # print(result2)
  # print(result)

  
  return(result)
}

test <- function(steps1, steps2, interpolation) {
  vanilla <- data.frame(type = "call", expiry = 1.0, qty = 1.0, strike = 100.0, stringsAsFactors = FALSE)
  prices <- seq(60.0, 150.0, 1.0)
  
  p1 <- mapply(
    function(price) CppPriceEuropeanUncertainVol(vanilla, 0.2, 0.2, 0.1, price, "bid", steps1, price * 2, interpolation)$value,
    prices)
  
  p2 <- mapply(
    function(price) CppPriceEuropeanUncertainVol(vanilla, 0.2, 0.2, 0.1, price, "bid", steps2, price * 2, interpolation)$value,
    prices)
  
  r <- mapply(
    function(price) Richardson(0.2, 0.2, 0.1, price, "bid", steps1, steps2, interpolation, vanilla),
    prices)
  
  bs <- mapply(
    function(price) EuropeanOption("call", price, 100.0, 0.0, 0.1, 1.0, 0.2)$value,
    prices)
  
  er <- abs((r - bs) / bs);
  ep1 <- abs((p1 - bs) / bs);
  ep2 <- abs((p2 - bs) / bs);
  
  data.frame(prices = prices, ep1 = ep1, ep2 = ep2, er = er)
}

p1 <- test(100, 120, "linear")
p2 <- test(100, 120, "cubic")

print(mean(p1$ep2 / p2$ep2))
print(mean(p1$er / p2$er))
print(mean(p1$ep2) / mean(p2$ep2))

ggplot() +
  geom_line(data = p1, aes(x = prices, y = ep2), color = 'blue') +
  geom_line(data = p2, aes(x = prices, y = ep2), color = 'red') +
  scale_y_log10() +
  xlab("Price") +
  ylab("Relative Error") +
  ggtitle("Finite difference error over price domain")
  

p1$interpolation <- "linear"
p2$interpolation <- "cubic"
pp <- rbind(p1, p2)

ggplot(pp, aes(x = prices, y = er, group=interpolation, color=interpolation)) +
  geom_line() +
  scale_y_log10() +
  xlab("Price") +
  ylab("Relative Error") +
  ggtitle("Finite difference error over price domain")



plots <- lapply(
  c("er", "ep1", "ep2"),
  function(v) {
    ggplot(pp, aes_string(x = "prices", y = v, group = "interpolation", color = "interpolation")) +
      geom_line() +
      scale_y_log10() +
      xlab("Price") +
      ylab("Relative Error") +
      ggtitle("Finite difference error over price domain")
  }
)

do.call(grid.arrange, plots)


result <- EuropeanFD(0.2, 0.2, 0.05, 100.0, "bid", 1000, vanilla)

ofInterest <- result$prices > 50.0 & result$prices < 150.0
values <- result$values[ofInterest]
prices <- result$prices[ofInterest]

bsValues <- mapply(
  function (price) EuropeanOption("call", price, 100.0, 0.0, 0.05, 1.0, 0.2)$value,
  prices)
errors <- values - bsValues
# plot(errors)
relErrors <- errors / bsValues
plot(relErrors)



exotic <- CreateBinaryCall(1.0, 100.0)
scenario <- CreateScenario(0.1, 0.3, underlyingPrice = 100.0)

opt <- OptimizeHedge(scenario, exotic)
Verify(scenario, exotic, opt$solution[1:2], opt$solution[3:4])

opt <- OptimizeHedge2(scenario, exotic)
Verify(scenario, exotic, opt$solution[1:2], c(95.0, 105.0))


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










library(Rcpp)
library(RQuantLib)

cxxflags <- paste0("-std=c++0x -Doverride= -I", getwd())
Sys.setenv("PKG_CXXFLAGS"=cxxflags)
sourceCpp("Rinterface.cpp")


options <- data.frame(
  type = "call",
  expiry = 1.0,
  qty = 1.0,
  strike = 100.0);

PriceOptions(0.1, 0.3, 0.05, 100.0, options)

# Compare with RQuantLib price
EuropeanOption("call", 100.0, 100.0, 0.0, 0.05, 1.0, 0.2)




minVol <- 0.1
maxVol <- 0.3
impliedVol <- 0.2
strike <- 100.0
overhedgeStrike <- 90.0
underlyingPrice <- 100.0
riskFreeRate <- 0.05

expiry <- 1.0


value <- function(
  overhedgeStrike,
  hedgeQty1 = -1.0 / (strike - overhedgeStrike),
  hedgeQty2 = 1.0 / (strike - overhedgeStrike)) {
  portfolio <- PriceHedgedBinary(minVol, maxVol, riskFreeRate, underlyingPrice, expiry, strike, overhedgeStrike, hedgeQty1, hedgeQty2)
  hedgeLeg1 <- EuropeanOption("call", underlyingPrice, overhedgeStrike, 0.0, riskFreeRate, expiry, impliedVol)
  hedgeLeg2 <- EuropeanOption("call", underlyingPrice, strike, 0.0, riskFreeRate, expiry, impliedVol)
  hedgeCost <- hedgeLeg1$value * hedgeQty1 + hedgeLeg2$value * hedgeQty2
  bid <- portfolio["bid"] + hedgeCost
  ask <- portfolio["ask"] + hedgeCost
  c(bid, ask)
}


values <- sapply(seq(0, 0.2, 0.01), function(x) value(95.0, -x, x))
spreads <- values[2,] - values[1,]
plot(spreads)

bids <- matrix(NA, 10, 10)
# asks <- matrix(NA, 10, 10)
spreads <- matrix(NA, 10, 10)

rownames(spreads) <- seq(-0.4, 0.5, 0.1)
colnames(spreads) <- seq(-0.4, 0.5, 0.1)

for (i in 1:10) {
  for (j in 1:10) {
    v <- value(95.0, i / 10 - 0.5, j / 10 - 0.5)
    spreads[i, j] <- v["ask"] - v["bid"]
    # bids[i, j] <- v["bid"]
    # asks[i, j] <- v["ask"]
  }
}  

image(spreads)
spreads



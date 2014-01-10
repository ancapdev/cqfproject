

# Calculate payoff at price
CalculatePayoff <- function(type, strike, price) {
  switch(
    type,
    call = max(price - strike, 0),
    put = max(strike - price, 0),
    bcall = if(price > strike) 1 else 0,
    bput = if(price < strike) 1 else 0)
}

# Calculate average payoff in interval [price1, price2]
CalculateAveragePayoff <- function(type, strike, price1, price2) {
  switch(
    type,
    
    # TODO: verify
    call =
      if(price1 > strike)
        0.5 * (price1 + price2) - strike
    else if (price2 > strike)
      0.5 * (price2 - strike)^2 / (price2 - price1)
    else
      0,
    
    put =
      if(price2 < strike)
        strike - 0.5 * (price1 + price2)
    else if (price1 < strike)
      0.5 * (strike - price1)^2 / (price2 - price1)
    else
      0,
    
    bcall =
      if(price1 > strike)
        1
    else if (price2 > strike)
      (price2 - strike) / (price2 - price1)
    else
      0,
    
    bput =
      if(price2 < strike)
        1
    else if (price1 < strike)
      (strike - price1) / (price2 - price1)
    else
      0)
}

# Calculate payoffs for an option portfolio over a range of prices
CalculatePayoffs <- function(options, prices) {
  sapply(
    prices,
    function(price) {
      sum(
        mapply(
          function(type, strike) CalculatePayoff(type, strike, price),
          options$type, options$strike,
          USE.NAMES = FALSE) * options$qty)
    })
}


# Calculate payoffs for an option portfolio over a range of price intervals
CalculateAveragePayoffs <- function(options, prices1, prices2) {
  mapply(
    function(price1, price2) {
      sum(
        mapply(
          function(type, strike) CalculateAveragePayoff(type, strike, price1, price2),
          options$type, options$strike,
          USE.NAMES = FALSE) * options$qty)
    },
    prices1, prices2)
}

# Generate ggplot2 payoff chart over a range of prices
ChartPayoffs <- function(options, prices, includePoints = TRUE) {
  constituents <- lapply(
    seq_along(options$type),
    function(i)
      data.frame(
        price = prices,
        option = paste(options$strike[i], options$type[i]),
        payoff = CalculatePayoffs(options[i,], prices)))

  portfolio <- data.frame(
    price = prices,
    option = 'portfolio',
    payoff = CalculatePayoffs(options, prices))
  
  all <- rbind(do.call(rbind, constituents), portfolio)

  p <- ggplot(all, aes(x = price, y = payoff, group = option, color = option)) +
    geom_line()
  
  if (includePoints)
    p <- p+ geom_point()
  
  return(p)
}

# Generate ggplot2 payoff chart over a range of prices using interval payoff calculations centered at prices
ChartAveragePayoffs <- function(options, prices, deltaPrice = prices[2] - prices[1]) {
  constituents <- lapply(
    seq_along(options$type),
    function(i)
      data.frame(
        price = prices,
        option = paste(options$strike[i], options$type[i]),
        payoff = CalculateAveragePayoffs(options[i,], prices - deltaPrice, prices + deltaPrice)))
  
  portfolio <- data.frame(
    price = prices,
    option = 'portfolio',
    payoff = CalculateAveragePayoffs(options, prices - deltaPrice, prices + deltaPrice))

  all <- rbind(do.call(rbind, constituents), portfolio)
  
  ggplot(all, aes(x = price, y = payoff, group = option, color = option)) +
    geom_line() + geom_point()
}
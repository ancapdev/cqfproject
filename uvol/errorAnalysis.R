

AnalyzeErrorsByStep <- function(scenario, option, steps = seq(20L, 250L, 10L)) {
  # FD values
  fd <- mapply(
    function(s)
      CppPriceEuropeanUncertainVol(
        option,
        scenario$impliedVol,
        scenario$impliedVol,
        scenario$riskFreeRate,
        scenario$underlyingPrice,
        "bid", # arbitrary since minVol = maxVol
        s,
        scenario$underlyingPrice * 2)$value,
    steps)
  
  # FD+Richardson values (using max steps = steps so that order complexity is the same as the plain FD sample)
  fdr <- mapply(
    function(s) PriceEuropeanUncertain(scenario, option, "bid", s / 2, s),
    steps)
  
  # BS values
  bs <- mapply(
    function(steps) PriceEuropeanBS(scenario, option),
    steps)
  
  # Compose data frame with results for charting
  result <- rbind(
    data.frame(
      steps = steps,
      error = abs((bs - fd) / bs),
      algorithm = "FD"),
    data.frame(
      steps = steps,
      error = abs((bs - fdr) / bs),
      algorithm = "FD+Richardson"))
  
  p <- ggplot(result, aes(x = steps, y = error, group=algorithm, color=algorithm)) +
    geom_line() +
    scale_y_log10() +
    xlab("Price Steps") +
    ylab("Relative Error") +
    ggtitle(paste("Finite difference errors for ATM", option$type, "at different grid resolutions"))
  
  return(p)
}

AnalyzeErrorsByGridPrice <- function(scenario, option, steps = 80) {
  # FD values and prices
  fd <- CppPriceEuropeanUncertainVol(
    option,
    scenario$impliedVol,
    scenario$impliedVol,
    scenario$riskFreeRate,
    scenario$underlyingPrice,
    "bid", # arbitrary since minVol = maxVol
    steps,
    scenario$underlyingPrice * 2,
    detail = 1)
  
  # Truncate bottom 10%, where relative error grows very large and ends in 0/0
  minPriceIdx <- length(fd$prices) / 10
  maxPriceIdx <- length(fd$prices)
  pricesOfInterest <- fd$prices[minPriceIdx : maxPriceIdx]
  valuesOfInterest <- fd$values[minPriceIdx : maxPriceIdx]
  
  # BS values
  bs <- mapply(
    function(price) PriceEuropeanBS(CreateScenario(scenario$minVol, scenario$maxVol, underlyingPrice=price), option),
    pricesOfInterest)
  
  # Compose data frame with results for charting
  result <-
    data.frame(
      prices = pricesOfInterest,
      error = abs((bs - valuesOfInterest) / bs))
  
  p <- ggplot(result, aes(x = prices, y = error)) +
    geom_line() +
    scale_y_log10() +
    xlab("Price") +
    ylab("Relative Error") +
    ggtitle(paste("Finite difference errors for ATM", option$type, "at different grid points"))
  
  return(p)
}

AnalyzeErrorsByStrike <- function(
  scenario,
  type,
  steps1 = 100,
  steps2 = 200,
  strikes = seq(scenario$underlyingPrice * 0.5,
                scenario$underlyingPrice * 1.5,
                length.out=101)) {
  
  # FD values
  fd <- mapply(
    function(s)
      CppPriceEuropeanUncertainVol(
        CreateOption(1.0, s, type),
        scenario$impliedVol,
        scenario$impliedVol,
        scenario$riskFreeRate,
        scenario$underlyingPrice,
        "bid", # arbitrary since minVol = maxVol
        steps2,
        scenario$underlyingPrice * 2)$value,
    strikes)
  
  # FD+Richardson values (using max steps = steps so that order complexity is the same as the plain FD sample)
  fdr <- mapply(
    function(s) PriceEuropeanUncertain(scenario, CreateOption(1.0, s, type), "bid", steps1, steps2),
    strikes)
  
  # BS values
  bs <- mapply(
    function(s) PriceEuropeanBS(scenario, CreateOption(1.0, s, type)),
    strikes)
  
  # Compose data frame with results for charting
  result <- rbind(
    data.frame(
      strikes = strikes,
      error = abs(fd - bs),
      relError = abs((bs - fd) / bs),
      algorithm = "FD"),
    data.frame(
      strikes = strikes,
      error = abs(fdr - bs),
      relError = abs((bs - fdr) / bs),
      algorithm = "FD+Richardson"))
  
  p <- ggplot(result, aes(x = strikes, y = relError, group=algorithm, color=algorithm)) +
    geom_line() +
    geom_line(stat = "hline", yintercept = mean) +
    scale_y_log10() +
    xlab("Strike") +
    ylab("Relative Error") +
    ggtitle(paste("Finite difference errors for", option$type, "at different strikes"))
  
  return(p)
}

AnalyzeErrors <- function() {
  dir.create(file.path(getwd(), "charts"), showWarnings = FALSE)
  chartType <- "pdf"
  
  scenario <- CreateScenario(0.2, 0.2)
  
  for (type in c("call", "put", "bcall", "bput")) {
    option <- CreateOption(1.0, scenario$underlyingPrice, type)

    # ATM at current price FD vs FD+Richardson vs BS for varying grid sizes
    ggsave(paste0("charts/error_steps_atm_", type, ".", chartType),
           AnalyzeErrorsByStep(scenario, option))

    # Option at current price FD vs FD+Richardson vs BS for varying strikes
    ggsave(paste0("charts/error_strike_", type, ".", chartType),
           AnalyzeErrorsByStrike(scenario, type))
    
    # ATM across grid prices FD vs BS
    ggsave(paste0("charts/error_gridprice_atm_", type, ".", chartType),
           AnalyzeErrorsByGridPrice(scenario, option))
    
    # ATM between grid prices FD linear vs FD cubic vs BS
  }
  # Note on odd vs even grid sizes (even always falls at grid for current price, odd always fall outside, assuming maxPrice = price * 2)
}
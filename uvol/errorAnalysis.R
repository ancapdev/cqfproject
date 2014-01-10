

AnalyzeErrorsByStep <- function(scenario, option, steps = seq(41L, 251L, 10L), richardsonOffset = 20L, interpolation = "cubic") {
  # FD values
  fd <- sapply(
    steps,
    function(s)
      CppPriceEuropeanUncertainVol(
        option,
        scenario$impliedVol,
        scenario$impliedVol,
        scenario$riskFreeRate,
        scenario$underlyingPrice,
        "bid", # arbitrary since minVol = maxVol
        s,
        scenario$underlyingPrice * 2,
        interpolation)$value)
  
  fd2 <- sapply(
    steps,
    function(s)
      CppPriceEuropeanUncertainVol(
        option,
        scenario$impliedVol,
        scenario$impliedVol,
        scenario$riskFreeRate,
        scenario$underlyingPrice,
        "bid", # arbitrary since minVol = maxVol
        s - richardsonOffset,
        scenario$underlyingPrice * 2,
        interpolation)$value)
  
  # FD+Richardson values (using max steps = steps so that order complexity is the same as the plain FD sample)
  fdr <- sapply(
    steps,
    function(s) PriceEuropeanUncertain(scenario, option, "bid", s - richardsonOffset, s, interpolation))
  
  # BS values
  bs <- PriceEuropeanBS(scenario, option)
  
  # Compose data frame with results for charting
  result <- rbind(
    data.frame(
      value = fd,
      algorithm = "FD"),
    data.frame(
      value = fd2,
      algorithm = "FD2"),
    data.frame(
      value = fdr,
      algorithm = "FD+Richardson"))
  
  result$steps <- steps
  result$error <- result$value - bs
  result$absError <- abs(result$error)
    
   p <- ggplot(result, aes(x = steps, y = absError, group=algorithm, color=algorithm)) +
     geom_line() +
     scale_y_log10() +
     xlab("Price Steps") +
     ylab("Abs Error") +
     ggtitle(paste("Finite difference errors for", option$strike, option$type, "at different grid resolutions"))

#   p <- ggplot(result, aes(x = steps, y = error, group=algorithm, color=algorithm)) +
#     geom_line() +
#     # scale_y_log10() +
#     # geom_hline(yintercept=bs) +
#     xlab("Price Steps") +
#     ylab("Error") +
#     ggtitle(paste("Finite difference errors for", option$strike, option$type, "at different grid resolutions"))
  
  
  return(p)
}

AnalyzeErrorsByGridPrice <- function(scenario, option, steps = 201) {
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
  strikes = seq(scenario$underlyingPrice * 0.5,
                scenario$underlyingPrice * 1.5,
                length.out=101)) {
  
  # FD values
  fd1 <- sapply(
    strikes,
    function(s)
      CppPriceEuropeanUncertainVol(
        CreateOption(1.0, s, type),
        scenario$impliedVol,
        scenario$impliedVol,
        scenario$riskFreeRate,
        scenario$underlyingPrice,
        "bid", # arbitrary since minVol = maxVol
        getOption('uvol.steps1'),
        scenario$underlyingPrice * 2)$value)
  
  fd2 <- sapply(
    strikes,
    function(s)
      CppPriceEuropeanUncertainVol(
        CreateOption(1.0, s, type),
        scenario$impliedVol,
        scenario$impliedVol,
        scenario$riskFreeRate,
        scenario$underlyingPrice,
        "bid", # arbitrary since minVol = maxVol
        getOption('uvol.steps2'),
        scenario$underlyingPrice * 2)$value)

  # FD+Richardson values (using max steps = steps so that order complexity is the same as the plain FD sample)
  fdr <- sapply(
    strikes,
    function(s) PriceEuropeanUncertain(scenario, CreateOption(1.0, s, type), "bid"))
  
  # BS values
  bs <- sapply(
    strikes,
    function(s) PriceEuropeanBS(scenario, CreateOption(1.0, s, type)))
  
  # Compose data frame with results for charting
  result <- rbind(
    data.frame(
      value = fd1,
      algorithm = "FD1"),
    data.frame(
      value = fd2,
      algorithm = "FD2"),
    data.frame(
      value = fdr,
      algorithm = "Richardson (FD1 + FD2)"))
  
  result$strikes <- strikes
  result$error <- result$value - bs
  result$relError <- result$error / bs

  p <- ggplot(result, aes(x = strikes, y = abs(relError), group=algorithm, color=algorithm)) +
    geom_line() +
    geom_line(stat = "hline", yintercept = mean) +
    scale_y_log10() +
    xlab("Strike") +
    ylab("Relative Error") +
    ggtitle(paste("Finite difference errors for", type, "at different strikes"))
   
  return(p)
}

AnalyzeErrors <- function() {
  dir.create(file.path(getwd(), "charts"), showWarnings = FALSE)
  chartType <- "pdf"
  
  scenario <- CreateScenario(0.2, 0.2)
  
  for (type in c("call", "put", "bcall", "bput")) {
    option <- CreateOption(1.0, scenario$underlyingPrice, type)

    # ATM at current price FD vs FD+Richardson vs BS for varying grid sizes (aligned to price)
    ggsave(paste0("charts/error_steps_atm_aligned_", type, ".", chartType),
           AnalyzeErrorsByStep(scenario, option, seq(30L, 250L, 10L)))
    
    # ATM at current price FD vs FD+Richardson vs BS for varying grid sizes (shifted from price)
    ggsave(paste0("charts/error_steps_atm_shift_", type, ".", chartType),
           AnalyzeErrorsByStep(scenario, option, seq(31L, 251L, 10L)))
    
    # ATM at current price FD vs FD+Richardson vs BS for varying grid sizes (detaile at high end)
    ggsave(paste0("charts/error_steps_atm_all_", type, ".", chartType),
           AnalyzeErrorsByStep(scenario, option, seq(200L, 220L, 2L)))

    # Option at current price FD vs FD+Richardson vs BS for varying strikes
    ggsave(paste0("charts/error_strike_", type, ".", chartType),
           AnalyzeErrorsByStrike(scenario, type))
    
    # ATM across grid prices FD vs BS
    ggsave(paste0("charts/error_gridprice_atm_", type, ".", chartType),
           AnalyzeErrorsByGridPrice(scenario, option))
    
    # TODO
    # ATM between grid prices FD linear vs FD cubic vs BS
  }
  # Note on odd vs even grid sizes (even always falls at grid for current price, odd always fall outside, assuming maxPrice = price * 2)
}
library(ggplot2)

if (!exists("CreateScenario", mode = "function")) source("utility.R")
if (!exists("PriceEuropeanBS", mode = "function")) source("pricing.R")
if (!exists("ChartPayoffs", mode = "function")) source("payoffAnalysis.R")

AnalyzeErrorsByStep <- function(scenario, option, steps1 = seq(20L, 150L, 10L), steps2 = steps1 * 2L, useAbsolute = FALSE,...) {
  # FD values
  fd <- sapply(
    steps1,
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
        ...)$value)
  
  fd2 <- sapply(
    steps2,
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
        ...)$value)
  
  # FD+Richardson values (using max steps = steps so that order complexity is the same as the plain FD sample)
  fdr <- mapply(
    # function(s) PriceEuropeanUncertainRichardson(scenario, option, "bid", s - richardsonOffset, s, interpolation))
    function(s1, s2) PriceEuropeanUncertainRichardson(scenario, option, "bid", s1, s2, ...),
    steps1, steps2)
  
  # BS values
  bs <- PriceEuropeanBS(scenario, option)
  
  # Compose data frame with results for charting
  result <- rbind(
    data.frame(
      value = fd,
      algorithm = "FD1"),
    data.frame(
      value = fd2,
      algorithm = "FD2"),
    data.frame(
      value = fdr,
      algorithm = "Richardson(FD1, FD2)"))
  
  result$steps <- steps2
  result$error <- result$value - bs
  result$absError <- abs(result$error)
  result$relError <- result$error / bs
  result$absRelError <- abs(result$relError)
 
  p <- if (useAbsolute) {
    ggplot(result, aes(x = steps, y = absError, group=algorithm, color=algorithm)) + ylab("Absolute Error")
  } else {
    ggplot(result, aes(x = steps, y = absRelError, group=algorithm, color=algorithm)) + ylab("Relative Error")
  }
  
  return (p +
    geom_line() +
    scale_y_log10() +
    scale_x_log10() +
    xlab("Price Steps"))
}

AnalyzeErrorsByGridPrice <- function(scenario, option, steps = 200, ...) {
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
    detail = 1,
    ...)
  
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
    ylab("Relative Error")
    # ggtitle(paste("Finite difference errors for ATM", option$type, "at different grid points"))
  
  return(p)
}

AnalyzeErrorsByStrike <- function(
  scenario,
  type,
  strikes = seq(scenario$underlyingPrice * 0.5,
                scenario$underlyingPrice * 1.5,
                length.out=101),
  ...) {
  
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
        scenario$underlyingPrice * 2,
        ...)$value)
  
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
        scenario$underlyingPrice * 2,
        ...)$value)

  # FD+Richardson values (using max steps = steps so that order complexity is the same as the plain FD sample)
  fdr <- sapply(
    strikes,
    function(s) PriceEuropeanUncertainRichardson(scenario, CreateOption(1.0, s, type), "bid", ...))
  
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
    ylab("Relative Error")
    # ggtitle(paste("Finite difference errors for", type, "at different strikes"))
   
  return(p)
}

AnalyzeErrors <- function() {
  dir.create(file.path(getwd(), "charts"), showWarnings = FALSE)
  chartType <- "pdf"
  
  scenario <- CreateScenario(0.2, 0.2)

  pb <- txtProgressBar(max = 2 * 4 * 5 + 4)
  
  myggsave <- function(fileName, plot) {
    ggsave(fileName, plot, width = 10, height = 6)
    setTxtProgressBar(pb, getTxtProgressBar(pb) + 1)
  }
  
  for (sampling in c("point", "interval")) {
    for (type in c("call", "put", "bcall", "bput")) {
      option <- CreateOption(1.0, scenario$underlyingPrice, type)
      
      # ATM for varying grid sizes (aligned to price - interpolation error)
      myggsave(paste0("charts/error_steps_atm_aligned_", sampling, "_", type, ".", chartType),
               AnalyzeErrorsByStep(scenario, option, seq(30L, 250L, 10L), payoffSampling = sampling))
      
      # ATM for varying grid sizes (shifted from price)
      myggsave(paste0("charts/error_steps_atm_shift_", sampling, "_", type, ".", chartType),
               AnalyzeErrorsByStep(scenario, option, seq(31L, 251L, 10L), payoffSampling = sampling))
      
      # ATM for varying grid sizes (detail)
      myggsave(paste0("charts/error_steps_atm_all_", sampling, "_", type, ".", chartType),
               AnalyzeErrorsByStep(scenario, option, seq(200L, 220L, 2L), payoffSampling = sampling))
  
      # Varying strikes
      myggsave(paste0("charts/error_strike_", sampling, "_", type, ".", chartType),
               AnalyzeErrorsByStrike(scenario, type, payoffSampling = sampling))
      
      # ATM across grid prices
      myggsave(paste0("charts/error_gridprice_atm_", sampling, "_", type, ".", chartType),
              AnalyzeErrorsByGridPrice(scenario, option, payoffSampling = sampling))
    }
  }  
  
  portfolio <- rbind(
    CreateBinaryCall(1, 100),
    CreateCall(1, 95, qty = -0.1),
    CreateCall(1, 105, qty = 0.1))
  
  # Classically hedged exotic portfolio error for varying grid sizes (aligned to price)
  myggsave(paste0("charts/error_steps_atm_aligned_hedged_binary1.", chartType),
           AnalyzeErrorsByStep(scenario, portfolio, seq(10L, 200L, 10L), payoffSampling = "point"))
  
  myggsave(paste0("charts/error_steps_atm_aligned_hedged_binary2.", chartType),
           AnalyzeErrorsByStep(scenario, portfolio, seq(10L, 200L, 10L), payoffSampling = "interval"))
  
  # Classically hedged exotic portfolio error for varying grid sizes (shifted from price)
  myggsave(paste0("charts/error_steps_atm_shift_hedged_binary.", chartType),
           AnalyzeErrorsByStep(scenario, portfolio, seq(31L, 451L, 10L), useAbsolute = TRUE))
  
  # Classically hedged exotic portfolio error for varying grid sizes (detailed)
  myggsave(paste0("charts/error_steps_atm_all_hedged_binary.", chartType),
           AnalyzeErrorsByStep(scenario, portfolio, seq(200L, 220L, 1L), useAbsolute = TRUE))
  
  close(pb)
  # Note on odd vs even grid sizes (even always falls at grid for current price, odd always fall outside, assuming maxPrice = price * 2)
}
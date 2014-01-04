

AnalyzeErrors <- function() {
  dir.create(file.path(getwd(), "charts"), showWarnings = FALSE)
  chartType <- "pdf"
  
  scenario <- CreateScenario(0.2, 0.2)
  
  for (type in c("call", "put", "bcall", "bput")) {
    option <- CreateOption(1.0, scenario$underlyingPrice, type)
    steps <- seq(20L, 100L, 10L)
    
    # FD prices
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
    
    # FD+Richardson prices (using max steps = steps so that order complexity is the same as the plain FD sample)
    fdr <- mapply(
      function(s) PriceEuropeanUncertain(scenario, option, "bid", s, s - 10L),
      steps)
    
    # BS prices
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
      xlab("Price steps") +
      ylab("Relative Error") +
      ggtitle(paste("Finite difference errors for ATM", type, "at different grid resolutions"))
    
    ggsave(paste0("charts/error_atm_", type, "_steps.", chartType), p)
  }
  
  # for call, put, bcall, bput, portfolio
    # ATM at current price FD vs FD+Richardson vs BS for varying grid sizes
    # ATM across grid prices FD vs FD+Richardson vs BS
    # ATM between grid prices FD linear vs FD cubic vs BS
    # Note on odd vs even grid sizes (even always falls at grid for current price, odd always fall outside, assuming maxPrice = price * 2)
  
}
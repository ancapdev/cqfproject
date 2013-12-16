
# Calculate returns
# TODO: Convert to excess returns relative to risk free rate

returns <-
  diff(
    log(
      do.call(cbind, lapply(mget(ls(markets), markets), function(x) x[,6])) # extract adjusted close
      ) # apply log
    )[-1,] # take difference and drop first



# Configuration
startDate <- '2009-01-01'
endDate <- '2013-12-01'
cacheFile <- "cache.RData"

# Read symbol list
s <- read.csv("Symbols.csv", as.is=T)

# Set up environment for data
markets <- new.env()

# Load cache if exists
if (file.exists(cacheFile)) {
  cat("Loading cached data\n")
  load(cacheFile, envir=markets)
}

# Download uncached
cat("Downloading uncached data\n")
getSymbols(
  s$Symbol[!(s$Symbol %in% ls(markets))],
  env=markets,
  auto.assign=T,
  from=startDate,
  to=endDate)

# Cache
cat("Storing data to cache\n")
save(list=ls(markets), envir=markets, file=cacheFile)


# 10 year treasury for risk free
# Other alternatives:
# ^IRX: 13 week treasury yield
# ^TNX: 10 year treasury yield
# ^TYX: 30 year treasury yield
# ^FVX: 5 year treasury yield
riskFree <- getSymbols("^TNX", auto.assign=F, from=startDate)



# Read symbol list
s <- read.csv("Symbols.csv", as.is=T)

# Set up environment for data
markets <- new.env()

# Load cache if exists
if (file.exists("cache.RData")) {
  cat("Loading cached data\n")
  load("cache.RData", envir=markets)
}

# Download uncached
cat("Downloading uncached data\n")
getSymbols(
  s$Symbol[!(s$Symbol %in% ls(markets))],
  env=markets,
  auto.assign=T,
  from='2009-01-01')

# Cache
cat("Storing data to cache\n")
save(list=ls(markets), envir=markets, file="cache.RData")

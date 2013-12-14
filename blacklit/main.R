s <- read.csv("Symbols.csv", as.is=T)

data <- new.env()
getSymbols(s$Symbol, env=data, auto.assign=T)
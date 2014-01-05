
# Function shape estimation
# http://www.sce.carleton.ca/faculty/chinneck/MProbe/MProbePaper2.pdf
# 2.1 Function Shape
DifferencesFromLines <- function(f, lowerBound, upperBound, numLines, numSamplesPerLine = 10) {
  L <- length(lowerBound)
  count <- 0
  interpFactors <- seq(0, 1, length.out = numSamplesPerLine + 2)[2:(numSamplesPerLine + 1)]
  
  allDifferences <- vector("numeric")
  
  pb <- txtProgressBar(max = numLines)
  
  repeat {
    p1 <- runif(L, lowerBound, upperBound)
    p2 <- runif(L, lowerBound, upperBound)
    v1 <- f(p1)
    v2 <- f(p2)
    
    differences <- sapply(
      interpFactors,
      function(k) {
        p <- p1 * (1 - k) + p2 * k
        iv <- v1 * (1 - k) + v2 * k
        v <- f(p)
        return(iv - v)
      })
    
    allDifferences <- c(allDifferences, differences)
    
    count <- count + 1
    if (count == numLines)
      break
    
    setTxtProgressBar(pb, count)
  }
  
  close(pb)
  
  return(allDifferences)
}

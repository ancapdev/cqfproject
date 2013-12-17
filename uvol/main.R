library(Rcpp)

cxxflags <- paste0("-std=c++0x -Doverride= -I", getwd())
Sys.setenv("PKG_CXXFLAGS"=cxxflags)
sourceCpp("finiteDifferencePricer.cpp")
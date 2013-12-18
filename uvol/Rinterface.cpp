#include <Rcpp.h>

#include "finiteDifferencePricer.hpp"

using namespace Rcpp;
using namespace CqfProject;

// [[Rcpp::export]]
NumericVector PriceBinary(
    double minVol,
    double maxVol,
    double riskFreeRate,
    double underlyingPrice,
    double timeToExpiry,
    double strike)
{
    FiniteDifferencePricer pricer(
        minVol,
        maxVol,
        riskFreeRate,
        underlyingPrice * Real(2),
        201); // TODO: Adjust this after fixing pricer logic

    BinaryCall const binCall(timeToExpiry, strike, 1.0);

    pricer.AddContract(&binCall);

    return NumericVector::create(
        _["bid"] = pricer.Valuate(underlyingPrice, Side::BID),
        _["ask"] = pricer.Valuate(underlyingPrice, Side::ASK));
}

// [[Rcpp::export]]
NumericVector PriceHedgedBinary(
    double minVol,
    double maxVol,
    double riskFreeRate,
    double underlyingPrice,
    double timeToExpiry,
    double strike,
    double overhedgeStrike,
    double hedgeQty1,
    double hedgeQty2)
{
    FiniteDifferencePricer pricer(
        minVol,
        maxVol,
        riskFreeRate,
        underlyingPrice * Real(2),
        201); // TODO: Adjust this after fixing pricer logic

    BinaryCall const binCall(timeToExpiry, strike, 1.0);
    Call const hedge1(timeToExpiry, overhedgeStrike, hedgeQty1);
    Call const hedge2(timeToExpiry, strike, hedgeQty2);

    pricer.AddContract(&binCall);
    pricer.AddContract(&hedge1);
    pricer.AddContract(&hedge2);

    return NumericVector::create(
        _["bid"] = pricer.Valuate(underlyingPrice, Side::BID),
        _["ask"] = pricer.Valuate(underlyingPrice, Side::ASK));
}

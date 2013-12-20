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

    pricer.AddContract(OptionContract(OptionContract::Type::BINARY_CALL, timeToExpiry, strike, 1.0));

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

    pricer.AddContract(OptionContract(OptionContract::Type::BINARY_CALL, timeToExpiry, strike, 1.0));
    pricer.AddContract(OptionContract(OptionContract::Type::CALL, timeToExpiry, overhedgeStrike, hedgeQty1));
    pricer.AddContract(OptionContract(OptionContract::Type::CALL, timeToExpiry, strike, hedgeQty2));

    return NumericVector::create(
        _["bid"] = pricer.Valuate(underlyingPrice, Side::BID),
        _["ask"] = pricer.Valuate(underlyingPrice, Side::ASK));
}

// [[Rcpp::export]]
NumericVector PriceOptions(
    double minVol,
    double maxVol,
    double riskFreeRate,
    double underlyingPrice,
    DataFrame options)
{
    FiniteDifferencePricer pricer(
        minVol,
        maxVol,
        riskFreeRate,
        underlyingPrice * Real(2),
        201); // TODO: Adjust this after fixing pricer logic

    CharacterVector type = options["type"];
    NumericVector expiry = options["expiry"];
    NumericVector qty = options["qty"];
    NumericVector strike = options["strike"];

    int const count = options.nrows();
    for (int i = 0; i < count; ++i)
    {
        // TODO: Factor out and check validity
        OptionContract::Type const t =
            type[i] == "call" ? OptionContract::Type::CALL :
            type[i] == "bcall" ? OptionContract::Type::BINARY_CALL :
            type[i] == "put" ? OptionContract::Type::PUT :
            OptionContract::Type::BINARY_PUT;

        pricer.AddContract(OptionContract(t, expiry[i], strike[i], qty[i]));
    }

    return NumericVector::create(
        _["bid"] = pricer.Valuate(underlyingPrice, Side::BID),
        _["ask"] = pricer.Valuate(underlyingPrice, Side::ASK));
}

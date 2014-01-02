#include <Rcpp.h>

#include <stdexcept>

#include "finiteDifferencePricer.hpp"

using namespace Rcpp;
using namespace CqfProject;

Side ToSide(std::string const& s)
{
    if (s == "bid")
        return Side::BID;
    else if (s == "ask")
        return Side::ASK;
    else
        throw std::runtime_error("invalid side");
}

Interpolation ToInterpolation(std::string const& s)
{
    if (s == "linear")
        return Interpolation::LINEAR;
    else if (s == "cubic")
        return Interpolation::CUBIC;
    else
        throw std::runtime_error("invalid interpolation");
}

template<typename StringType>
OptionContract::Type ToContractType(StringType s)
{
    if (s == "call")
        return OptionContract::Type::CALL;
    else if (s == "bcall")
        return OptionContract::Type::BINARY_CALL;
    else if (s == "put")
        return OptionContract::Type::PUT;
    else if (s == "bput")
        return OptionContract::Type::BINARY_PUT;
    else
        throw std::runtime_error("invalid contract type");
}

void PopulateContracts(FiniteDifferencePricer& pricer, DataFrame const& options)
{
    CharacterVector type = options["type"];
    NumericVector expiry = options["expiry"];
    NumericVector qty = options["qty"];
    NumericVector strike = options["strike"];

    int const count = options.nrows();
    for (int i = 0; i < count; ++i)
        pricer.AddContract(
            OptionContract(
                ToContractType(type[i]),
                expiry[i],
                strike[i],
                qty[i]));
}

// returns dataframe with
// - prices : NumericVector of underlying prices
// - values : NumericVector of portfolio values at t=0
// [[Rcpp::export]]
DataFrame EuropeanFD(
    double minVol,
    double maxVol,
    double riskFreeRate,
    double underlyingPrice,
    std::string side,
    int numPriceSteps,
    DataFrame options)
{
    FiniteDifferencePricer pricer(
        minVol,
        maxVol,
        riskFreeRate,
        underlyingPrice * Real(2), // TODO: should be a scaled depending on max vol and time horizing
        numPriceSteps);

    PopulateContracts(pricer, options);

    NumericVector prices(pricer.GetPrices().begin(), pricer.GetPrices().end());

    // Note: NumericVector doesn't work with std::back_inserter. Sub-optimal work around:
    //       Pre-allocating and initializing elements here and use regular iterator over elements to write values.
    NumericVector values(prices.size());

    double const value = pricer.Valuate(underlyingPrice, ToSide(side), values.begin());

    return DataFrame::create(
        _["prices"] = prices,
        _["values"] = values);
}

// [[Rcpp::export]]
NumericVector PriceOptionsNew(
    double minVol,
    double maxVol,
    double riskFreeRate,
    double underlyingPrice,
    std::string side,
    int priceSteps,
    std::string interpolation,
    DataFrame options)
{
    FiniteDifferencePricer pricer(
        minVol,
        maxVol,
        riskFreeRate,
        underlyingPrice * Real(2), // TODO: should be a scaled depending on max vol and time horizing
        priceSteps,
        ToInterpolation(interpolation));

    PopulateContracts(pricer, options);

    return NumericVector::create(pricer.Valuate(underlyingPrice, ToSide(side)));
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
        underlyingPrice * Real(2), // TODO: should be a scaled depending on max vol and time horizing
        201); // TODO: Adjust this after fixing pricer logic

    PopulateContracts(pricer, options);

    return NumericVector::create(
        _["bid"] = pricer.Valuate(underlyingPrice, Side::BID),
        _["ask"] = pricer.Valuate(underlyingPrice, Side::ASK));
}

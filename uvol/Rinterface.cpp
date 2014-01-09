#include <Rcpp.h>

#include <stdexcept>

#include "finiteDifferencePricer.hpp"
#include "blackScholes.hpp"

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

// [[Rcpp::export]]
List CppPriceEuropeanUncertainVol(
    DataFrame options,
    double minVol,
    double maxVol,
    double riskFreeRate,
    double underlyingPrice,
    std::string side,
    int priceSteps,
    double maxPrice,
    std::string interpolation = "cubic",
    int detail = 0)
{
    FiniteDifferencePricer pricer(
        minVol,
        maxVol,
        riskFreeRate,
        maxPrice,
        priceSteps,
        ToInterpolation(interpolation));

    PopulateContracts(pricer, options);

    if (detail >= 1)
    {
        NumericVector prices(pricer.BeginPrices(), pricer.EndPrices());

        // Note: NumericVector doesn't work with std::back_inserter. Sub-optimal work around:
        std::vector<double> values;

        double const value = pricer.Valuate(underlyingPrice, ToSide(side), std::back_inserter(values), detail);

        return List::create(
            _["value"] = NumericVector::create(value),
            _["prices"] = prices,
            _["values"] = NumericVector(values.begin(), values.end()));
    }
    else
    {
        return List::create(
            _["value"] = NumericVector::create(
                pricer.Valuate(underlyingPrice, ToSide(side))));
    }
}

// [[Rcpp::export]]
double CppPriceEuropeanBS(
    DataFrame options,
    double vol,
    double riskFreeRate,
    double underlyingPrice)
{
    CharacterVector type = options["type"];
    NumericVector expiry = options["expiry"];
    NumericVector qty = options["qty"];
    NumericVector strike = options["strike"];

    int const count = options.nrows();
    double sum = 0.0;
    for (int i = 0; i < count; ++i)
    {
        double const T = expiry[i];
        double const K = strike[i];
        double const q = qty[i];

        PutCallPair const putCall = BlackScholesPutCall(vol, riskFreeRate, T, underlyingPrice, K);

        if (type[i] == "call")
            sum += q * putCall.call;
        else if (type[i] == "put")
            sum += q * putCall.put;
        else if (type[i] == "bcall")
            sum += q * BlackScholesBinaryCall(vol, riskFreeRate, T, underlyingPrice, K);
        else if (type[i] == "bput")
            sum += q * BlackScholesBinaryPut(vol, riskFreeRate, T, underlyingPrice, K);
        else
            throw std::runtime_error("Invalid option type");
    }

    return sum;
}


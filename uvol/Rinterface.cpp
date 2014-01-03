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

    if (detail > 0)
    {
        NumericVector prices(pricer.GetPrices().begin(), pricer.GetPrices().end());

        // Note: NumericVector doesn't work with std::back_inserter. Sub-optimal work around:
        //       Pre-allocating and initializing elements here and use regular iterator over elements to write values.
        NumericVector values(prices.size());

        double const value = pricer.Valuate(underlyingPrice, ToSide(side), values.begin());

        return List::create(
            _["value"] = NumericVector::create(value),
            _["prices"] = prices,
            _["values"] = values);
    }
    else
    {
        return List::create(
            _["value"] = NumericVector::create(
                pricer.Valuate(underlyingPrice, ToSide(side))));
    }
}

#include "fd.hpp"

#include <iostream>

#include <boost/math/distributions/normal.hpp>

namespace CqfProject
{
    double Phi(double x)
    {
        return boost::math::cdf(boost::math::normal(), x);
    }

    struct PutCallPair
    {
        double call;
        double put;
    };

    PutCallPair BlackScholesPutCall(
        double vol,
        double rate,
        double timeToExpiry,
        double price,
        double strike)
    {
        double const d1 = (1.0 / (vol * std::sqrt(timeToExpiry))) * (std::log(price / strike) + (rate + 0.5 * vol * vol) * timeToExpiry);
        double const d2 = d1 - vol * timeToExpiry;

        PutCallPair values;
        values.call = Phi(d1) * price - Phi(d2) * strike * std::exp(-rate * timeToExpiry);
        values.put = strike * std::exp(-rate * timeToExpiry) - price + values.call;
        return values;
    }
}

int main()
{
    using namespace CqfProject;

    std::vector<OptionContract> contracts;
    // contracts.push_back(OptionContract::Call(1.0, 100.0, 1.0));
    contracts.push_back(OptionContract::Put(1.0, 100.0, 1.0));
    // contracts.push_back(OptionContract::BinaryCall(1.0, 100, 1.0));

    auto value = PricePortfolio(
        0.20,
        0.20,
        0.05,
        200.0,
        100.0,
        1.0,
        0.01,
        contracts);
    
    std::cout << std::get<0>(value) << std::endl;
    std::cout << std::get<1>(value) << std::endl;

    auto const prices = BlackScholesPutCall(0.20, 0.05, 1.0, 100.0, 100.0);

    std::cout << "BS Call: " << prices.call << std::endl;
    std::cout << "BS Put:  " << prices.put << std::endl;
    
    return 0;

}
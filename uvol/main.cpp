#include "fd.hpp"

#include "nlopt.hpp"

#include <boost/math/distributions/normal.hpp>

#include <iostream>

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
        // std::cout << "Call @ " << strike << ": " << values.call << std::endl;
        return values;
    }
}

int main()
{
    // NOTE: http://www.deltaquants.com/overhedging.html

    using namespace CqfProject;


    double const rate = 0.05;
    double const price = 100.0;
    double const strike = 100.0;
    double const overhedgeStrike = 90.0;
    double const hedgeQty = 1.0 / (strike - overhedgeStrike);
    double const minVol = 0.10;
    double const maxVol = 0.30;
    double const impliedVol = 0.20;
    double const timeToExpiry = 1.0;


    std::vector<OptionContract> contracts;
    contracts.push_back(OptionContract::BinaryCall(timeToExpiry, strike, 1.0));

    auto unhedgedValue = PricePortfolio(
        minVol,
        maxVol,
        rate,
        price * 2.0,
        price,
        1.0,
        0.01,
        contracts);

    std::cout << "Unhedged bid: " << std::get<0>(unhedgedValue) << std::endl;
    std::cout << "Unhedged ask: " << std::get<1>(unhedgedValue) << std::endl;

    contracts.push_back(OptionContract::Call(timeToExpiry, overhedgeStrike, -hedgeQty));
    contracts.push_back(OptionContract::Call(timeToExpiry, strike, hedgeQty));

    auto value = PricePortfolio(
        minVol,
        maxVol,
        rate,
        price * 2.0,
        price,
        1.0,
        0.01,
        contracts);
    
    std::cout << "Portfolio bid: " << std::get<0>(value) << std::endl;
    std::cout << "Portfolio ask: " << std::get<1>(value) << std::endl;

    auto const prices1  = BlackScholesPutCall(impliedVol, rate, timeToExpiry, price, overhedgeStrike);
    auto const prices2 = BlackScholesPutCall(impliedVol, rate, timeToExpiry, price, strike);
    double const callSpreadPrice = (prices1.call - prices2.call) * hedgeQty;
    std::cout << "Call spread price: " << callSpreadPrice << std::endl;
    
    std::cout << "Binary bid: " << std::get<0>(value) + callSpreadPrice << std::endl;
    std::cout << "Binary ask: " << std::get<1>(value) + callSpreadPrice << std::endl;
    
    
    nlopt::opt optimizer(nlopt::GN_DIRECT_L, 2);
    
    auto objectivFunc = [] (std::vector<double> const& x, std::vector<double>&, void*) -> double
    {
        return x[0] * x[0] + x[1] * x[1];
    };

    optimizer.set_min_objective(objectivFunc, nullptr);
    optimizer.set_lower_bounds(std::vector<double>(2, -1.0));
    optimizer.set_upper_bounds(std::vector<double>(2, 1.0));
    optimizer.set_maxeval(100);


    std::vector<double> x(2, 0.0);
    double optimizedValue = 0.0;
    nlopt::result result = optimizer.optimize(x, optimizedValue);


    return 0;

}
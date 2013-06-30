#ifndef UVOL_FD_HPP
#define UVOL_FD_HPP

#include <functional>
#include <tuple>
#include <vector>

namespace CqfProject
{
    struct OptionContract
    {
        typedef std::function<double (double price)> PayoffFunction;

        OptionContract(double expiry, PayoffFunction const& payoff)
            : expiry(expiry)
            , payoff(payoff)
        {}

        static OptionContract Call(double expiry, double strike, double multiplier);
        static OptionContract Put(double expiry, double strike, double multiplier);
        static OptionContract BinaryCall(double expiry, double strike, double multiplier);

        double expiry;
        PayoffFunction payoff;
    };

    std::tuple<double, double> PricePortfolio(
        double minVol,
        double maxVol,
        double rate,
        double currentPrice,
        double maxPrice,
        double targetDeltaPrice,
        double targetDeltaTime,
        std::vector<OptionContract> contracts);
    
    /*

    For multiple expiries, could have a partial evaluation function which returns the whole column of the grid. 
    The column could be used as payoffs + payoff of newly expired in next partial
    */

    // payoffFunc(price)
    // valueFunc(value, price, time)
    template<typename ValueFunc, typename PayoffFunc>
    double FiniteDifferenceValue(
        double maxPrice,
        double deltaPrice,
        double maxTime,
        double deltaTime,
        double minVol,
        double maxVol,
        double rate,
        ValueFunc valueFunc,
        PayoffFunc payoffFunc)
    {
        typedef std::vector<double> Column;

        Column current;

  
        return 0.0;
    }
}

#endif

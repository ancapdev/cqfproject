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
    
    enum class Side
    {
        BID,
        ASK
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

    double PricePortfolio(
        double minVol,
        double maxVol,
        double rate,
        double currentPrice,
        double maxPrice,
        double targetDeltaPrice,
        double targetDeltaTime,
        Side side,
        std::vector<OptionContract> contracts);
}

#endif

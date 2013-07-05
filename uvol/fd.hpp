#ifndef UVOL_FD_HPP
#define UVOL_FD_HPP

#include <functional>
#include <tuple>
#include <vector>

namespace CqfProject
{
    typedef double Real;

    struct OptionContract
    {
        typedef std::function<Real (Real price)> PayoffFunction;

        OptionContract(Real expiry, PayoffFunction const& payoff)
            : expiry(expiry)
            , payoff(payoff)
        {}

        static OptionContract Call(Real expiry, Real strike, Real multiplier);
        static OptionContract Put(Real expiry, Real strike, Real multiplier);
        static OptionContract BinaryCall(Real expiry, Real strike, Real multiplier);

        Real expiry;
        PayoffFunction payoff;
    };
    
    enum class Side
    {
        BID,
        ASK
    };

    std::tuple<Real, Real> PricePortfolio(
        Real minVol,
        Real maxVol,
        Real rate,
        Real currentPrice,
        Real maxPrice,
        Real targetDeltaPrice,
        Real targetDeltaTime,
        std::vector<OptionContract> const& contracts);

    Real PricePortfolio(
        Real minVol,
        Real maxVol,
        Real rate,
        Real currentPrice,
        Real maxPrice,
        Real targetDeltaPrice,
        Real targetDeltaTime,
        Side side,
        std::vector<OptionContract> const& contracts);
}

#endif

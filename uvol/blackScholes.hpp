#ifndef UVOL_BLACK_SCHOLES_HPP
#define UVOL_BLACK_SCHOLES_HPP

#include "types.hpp"

#include <boost/math/distributions/normal.hpp>

namespace CqfProject
{
    Real Phi(Real x)
    {
        return (Real)boost::math::cdf(boost::math::normal(), x);
    }

    struct PutCallPair
    {
        Real call;
        Real put;
    };

    PutCallPair BlackScholesPutCall(
        Real vol,
        Real rate,
        Real timeToExpiry,
        Real price,
        Real strike)
    {
        Real const d1 = (Real(1) / (vol * std::sqrt(timeToExpiry))) * (std::log(price / strike) + (rate + Real(0.5) * vol * vol) * timeToExpiry);
        Real const d2 = d1 - vol * timeToExpiry;

        PutCallPair values;
        values.call = Phi(d1) * price - Phi(d2) * strike * std::exp(-rate * timeToExpiry);
        values.put = strike * std::exp(-rate * timeToExpiry) - price + values.call;
        // std::cout << "Call @ " << strike << ": " << values.call << std::endl;
        return values;
    }
}

#endif

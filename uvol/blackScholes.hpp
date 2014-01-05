#ifndef UVOL_BLACK_SCHOLES_HPP
#define UVOL_BLACK_SCHOLES_HPP

#include "types.hpp"

// #include <boost/math/distributions/normal.hpp>

namespace CqfProject
{
/*
    Real Phi(Real x)
    {
        return (Real)boost::math::cdf(boost::math::normal(), x);
    }
*/

    // From http://www.johndcook.com/cpp_phi.html
    inline double Phi(double x)
    {
        // constants
        double a1 =  0.254829592;
        double a2 = -0.284496736;
        double a3 =  1.421413741;
        double a4 = -1.453152027;
        double a5 =  1.061405429;
        double p  =  0.3275911;

        // Save the sign of x
        int sign = 1;
        if (x < 0)
            sign = -1;
        x = fabs(x)/sqrt(2.0);

        // A&S formula 7.1.26
        double t = 1.0/(1.0 + p*x);
        double y = 1.0 - (((((a5*t + a4)*t) + a3)*t + a2)*t + a1)*t*exp(-x*x);

        return 0.5*(1.0 + sign*y);
    }

    struct PutCallPair
    {
        Real call;
        Real put;
    };

    struct BlackScholesCoefficients
    {
        BlackScholesCoefficients(
            Real vol,
            Real rate,
            Real timeToExpiry,
            Real price,
            Real strike)
            : d1((std::log(price / strike) + (rate + Real(0.5) * vol * vol) * timeToExpiry) / (vol * std::sqrt(timeToExpiry)))
            , d2(d1 - vol * std::sqrt(timeToExpiry))
            , D(std::exp(-rate * timeToExpiry))
        {}

        Real d1;
        Real d2;
        Real D;
    };

    inline PutCallPair BlackScholesPutCall(
        Real vol,
        Real rate,
        Real timeToExpiry,
        Real price,
        Real strike)
    {
        BlackScholesCoefficients const c(vol, rate, timeToExpiry, price, strike);

        PutCallPair values;
        values.call = Phi(c.d1) * price - Phi(c.d2) * strike * c.D;
        values.put = strike * c.D - price + values.call;
        return values;
    }

    inline Real BlackScholesBinaryCall(
        Real vol,
        Real rate,
        Real timeToExpiry,
        Real price,
        Real strike)
    {
        BlackScholesCoefficients const c(vol, rate, timeToExpiry, price, strike);
        return c.D * Phi(c.d2);
    }

    inline Real BlackScholesBinaryPut(
        Real vol,
        Real rate,
        Real timeToExpiry,
        Real price,
        Real strike)
    {
        BlackScholesCoefficients const c(vol, rate, timeToExpiry, price, strike);
        return c.D * Phi(-c.d2);
    }
}

#endif

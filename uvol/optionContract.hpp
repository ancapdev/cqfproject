#ifndef OPTION_CONTRACT_HPP
#define OPTION_CONTRACT_HPP

#include "types.hpp"

namespace CqfProject
{
    struct OptionContract
    {
        typedef OptionType Type;

        OptionContract(Type type, Real expiry, Real strike, Real multiplier)
            : type(type)
            , expiry(expiry)
            , strike(strike)
            , multiplier(multiplier)
        {}

        // TODO: Flag to switch on point sampling
        // Calculates average payoff in the interval [price1, price2)
        Real CalculateAveragePayoff(Real price1, Real price2) const
        {
            switch (type)
            {
            case Type::CALL:
                // return Real(0.5) * (std::max(price1 - strike, Real(0)) + std::max(price2 - strike, Real(0)));
                return price1 > strike
                    ? Real(0.5) * (price1 + price2) - strike
                    : price2 > strike
                    ? Real(0.5) * (price2 - strike) * (price2 - strike) / (price2 - price1)
                    : Real(0);

            case Type::PUT:
                // return Real(0.5) * (std::max(strike - price1, Real(0)) + std::max(strike - price2, Real(0)));
                return price2 < strike
                    ? strike - Real(0.5) * (price1 + price2)
                    : price1 < strike
                    ? Real(0.5) * (strike - price1) * (strike - price1) / (price2 - price1)
                    : Real(0);
                                

            case Type::BINARY_CALL:
                return price1 > strike
                    ? Real(1)
                    : price2 > strike
                    ? (price2 - strike) / (price2 - price1)
                    : Real(0);

            case Type::BINARY_PUT:
                return price2 < strike
                    ? Real(1)
                    : price1 < strike
                    ? (strike - price1) / (price2 - price1)
                    : Real(0);

            default:
                throw std::runtime_error("invalid option type");
            }
        }

        Type type;
        Real expiry;
        Real strike;
        Real multiplier;
    };
}

#endif

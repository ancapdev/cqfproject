#ifndef UVOL_FINITE_DIFFERENCE_PRICER_HPP
#define UVOL_FINITE_DIFFERENCE_PRICER_HPP

#include "types.hpp"

#include <algorithm>
#include <cassert>
#include <cstdint>
#include <stdexcept>
#include <vector>

namespace CqfProject
{
    struct OptionContract
    {
        enum class Type
        {
            PUT,
            CALL,
            BINARY_PUT,
            BINARY_CALL
        };

        OptionContract(Type type, Real expiry, Real strike, Real multiplier)
            : type(type)
            , expiry(expiry)
            , strike(strike)
            , multiplier(multiplier)
        {}

        Real CalculatePayoff(Real price) const
        {
            switch (type)
            {
            case Type::PUT:
                return std::max(strike - price, Real(0));                

            case Type::CALL:
                return std::max(price - strike, Real(0));

            case Type::BINARY_PUT:
                return price < strike ? 1.0 : 0.0;

            case Type::BINARY_CALL:
                return price > strike ? 1.0 : 0.0;

            default:
                abort();
            }
        }

        Type type;
        Real expiry;
        Real strike;
        Real multiplier;
    };

    enum class Side
    {
        BID,
        ASK
    };

    enum class Interpolation
    {
        LINEAR,
        CUBIC
    };

    class FiniteDifferencePricer
    {
    public:
        struct NullOutIt
        {
            struct NoAssign
            {
                NoAssign& operator = (Real rhs) { return *this; }
            };

            NoAssign operator * () { return NoAssign(); }

            NullOutIt& operator ++ (int) { return *this; }
        };

        FiniteDifferencePricer(
            Real minVol,
            Real maxVol,
            Real rate,
            Real maxPrice,
            std::size_t numPriceSteps,
            Interpolation interpolation = Interpolation::LINEAR)
            : mMinVol(minVol)
            , mMaxVol(maxVol)
            , mRate(rate)
            , mMaxPrice(maxPrice)
            , mNumPriceSteps(numPriceSteps)
            , mInterpolation(interpolation)
            , mDeltaPrice(maxPrice / numPriceSteps)
              // TODO: Verify stability condition and optimal time step size
            , mTargetDeltaTime(Real(0.99) / (numPriceSteps * numPriceSteps * maxVol * maxVol))
        {
            assert(maxVol >= minVol);

            // Cache prices
            mPrices.reserve(mNumPriceSteps + 1);
            for (std::size_t i = 0; i <= mNumPriceSteps; ++i)
                mPrices.push_back(i * mDeltaPrice);

            mScratch.resize((mNumPriceSteps + 1) * 2, Real(0));
        }

        void AddContract(OptionContract const& contract)
        {
            mContracts.push_back(contract);
        }

        std::vector<Real> const& GetPrices() const
        {
            return mPrices;
        }

        Real Valuate(Real price, Side side)
        {
            return Valuate(price, side, NullOutIt());
        }

        template<typename OutIt>
        Real Valuate(Real price, Side side, OutIt valuesOut)
        {
            // Maintain contracts sorted by descending expiry
            auto const expiryGreater = [] (OptionContract const& a, OptionContract const& b) { return a.expiry > b.expiry; };
            if (!std::is_sorted(mContracts.begin(), mContracts.end(), expiryGreater))
                std::sort(mContracts.begin(), mContracts.end(), expiryGreater);

            Real const minVolSq = mMinVol * mMinVol;
            Real const maxVolSq = mMaxVol * mMaxVol;

            if (side == Side::BID)
            {
                // Minimum portfolio value
                return ValuateImpl(
                    price,
                    [=] (Real gamma) -> Real
                    {
                        // NOTE: Rather than select on gamma > 0 directly, use a min/max like construct which is vectorizable
                        //       Equivalent would be 
                        //       return gamma > 0.0 ? minVolSq * gamma : maxVolSq * gamma;
                        Real const g1 = gamma * minVolSq;
                        Real const g2 = gamma * maxVolSq;
                        return g1 < g2 ? g1 : g2;
                    },
                    valuesOut);
            }
            else
            {
                // Maximum portfolio value
                return ValuateImpl(
                    price,
                    [=] (Real gamma) -> Real
                    {
                        Real const g1 = gamma * minVolSq;
                        Real const g2 = gamma * maxVolSq;
                        return g1 > g2 ? g1 : g2;
                    },
                    valuesOut);
            }
        }

    private:
        template<typename VolSqGammaFun, typename OutIt>
        Real ValuateImpl(Real price, VolSqGammaFun const& volSqGammaFun, OutIt valuesOut)
        {
            std::size_t numPriceSteps = mNumPriceSteps;
            Real const deltaPrice = mDeltaPrice;
            Real const deltaPriceSq = deltaPrice * deltaPrice;
            Real const invTwoDeltaPrice = Real(0.5) / deltaPrice;
            Real const invDeltaPriceSq = Real(1) / deltaPriceSq;

            Real const rate = mRate;

            Real const* __restrict prices = &mPrices[0];

            // Initial state
            Real* __restrict current = &mScratch[0];
            Real* __restrict next = &mScratch[numPriceSteps + 1];
            for (std::size_t i = 0; i <= numPriceSteps; ++i)
                current[i] = Real(0);

            // March from last expiry to next expiry or to 0
            for (std::size_t contractIndex = 0; contractIndex < mContracts.size(); ++contractIndex)
            {
                OptionContract const& contract = mContracts[contractIndex];

                // Add payoffs
                for (std::uint32_t i = 0; i <= numPriceSteps; ++i)
                    current[i] += contract.CalculatePayoff(prices[i]) * contract.multiplier;

                // Find next expiry and time to it
                Real const nextExpiry = contractIndex == mContracts.size() - 1 ? Real(0) : mContracts[contractIndex + 1].expiry;
                Real const timeToNextExpiry = contract.expiry - nextExpiry;

                // If contracts are too close together, assume at same expiry
                // This can break under intentionally bad data, but shouldn't in practice
                if (timeToNextExpiry < mTargetDeltaTime)
                    continue;

                // Simulate to next expiry or 0
                std::size_t const timeSteps = static_cast<std::size_t>(timeToNextExpiry / mTargetDeltaTime) + 1;
                Real const deltaTime = timeToNextExpiry / (timeSteps - 1);

                for (std::size_t k = 0; k < timeSteps; ++k)
                {
                    // Main grid
                    for (std::size_t i = 1; i < numPriceSteps; ++i)
                    {
                        Real const price = prices[i];
                        Real const delta = (current[i+1] - current[i-1]) * invTwoDeltaPrice;
                        Real const gamma = (current[i+1] - Real(2) * current[i] + current[i-1]) * invDeltaPriceSq;
                        Real const theta = rate * current[i] - Real(0.5) * volSqGammaFun(gamma) * price * price - rate * price * delta;
                        next[i] = current[i] - deltaTime * theta;
                    }

                    // Boundaries
                    next[0] = (Real(1) - rate * deltaTime) * current[0];
                    // TODO: check numPriceSteps >= 3
                    next[numPriceSteps] = Real(2) * next[numPriceSteps - 1] - next[numPriceSteps - 2];

                    // next.swap(current);
                    std::swap(next, current);
                }
            }


            // Copy out values
            for (std::uint32_t i = 0; i <= numPriceSteps; ++i)
                *valuesOut++ = current[i];

            // Find closest point above current price
            auto it = std::upper_bound(mPrices.begin(), mPrices.end(), price);
            if (it == mPrices.end())
                throw std::runtime_error("Current price not in simulated set");

            auto const index = it - mPrices.begin();

            // If at 0.0, return lowest price value
            if (index == 0)
                return current[0];

            // Interpolate between prices above and below current price
            if (mInterpolation == Interpolation::CUBIC &&
                index > 1u &&
                index < mPrices.size() - 1)
            {
                Real const v00 = current[index - 2];
                Real const v0 = current[index - 1];
                Real const v1 = current[index];
                Real const v11 = current[index + 1];
                Real const k = (prices[index] - price) / deltaPrice;
                return CubicInterpolate(v00, v0, v1, v11, Real(1) - k);
            }
            else
            {
                Real const v0 = current[index - 1];
                Real const v1 = current[index];
                Real const k = (prices[index] - price) / deltaPrice;
                return v0 * k + v1 * (Real(1) - k);
            }
        }

        // TODO: re-style
        static Real CubicInterpolate(
            Real y0, Real y1,
            Real y2, Real y3,
            Real mu)
        {
            Real a0,a1,a2,a3,mu2;

            mu2 = mu*mu;
            a0 = y3 - y2 - y0 + y1;
            a1 = y0 - y1 - a0;
            a2 = y2 - y0;
            a3 = y1;

            return(a0*mu*mu2+a1*mu2+a2*mu+a3);
        }

        //
        // User provided parameters
        // 
        Real mMinVol;
        Real mMaxVol;
        Real mRate;
        Real mMaxPrice;
        std::size_t mNumPriceSteps;
        Interpolation mInterpolation;
        std::vector<OptionContract> mContracts;

        //
        // Inferred parameters
        // 

        /// dS
        Real mDeltaPrice;

        /// dt small enough to meet stability condition given dS
        Real mTargetDeltaTime;

        //
        // Work space and cache, to avoid repeated allocation and calculations
        //
        std::vector<Real> mPrices;
        std::vector<Real> mScratch;
    };
}

#endif

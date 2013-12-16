#include "finiteDifferencePricer.hpp"

#include <algorithm>
#include <cassert>
#include <cstdint>
#include <stdexcept>

namespace CqfProject
{
    FiniteDifferencePricer::FiniteDifferencePricer(
        Real minVol,
        Real maxVol,
        Real rate,
        Real maxPrice,
        std::size_t numPriceSteps)
        : mMinVol(minVol)
        , mMaxVol(maxVol)
        , mRate(rate)
        , mMaxPrice(maxPrice)
        , mNumPriceSteps(numPriceSteps)
        , mDeltaPrice(maxPrice / (numPriceSteps - 1))
        // TODO: Verify stability condition and optimal time step size
        , mTargetDeltaTime(Real(0.99) / (numPriceSteps * numPriceSteps * maxVol * maxVol))
    {
        assert(maxVol >= minVol);

        // Cache prices
        mPrices.reserve(mNumPriceSteps);
        for (std::size_t i = 0; i < mNumPriceSteps; ++i)
            mPrices.push_back(i * mDeltaPrice);

        mScratch.resize(mNumPriceSteps * 2, Real(0));
    }
        

    void FiniteDifferencePricer::AddContract(OptionContract const* contract)
    {
        mContracts.push_back(contract);
    }

    Real FiniteDifferencePricer::Valuate(Real price, Side side)
    {
        // Maintain contracts sorted by descending expiry
        auto const expiryGreater = [] (OptionContract const* a, OptionContract const* b) { return a->expiry > b->expiry; };
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
                });
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
                });
        }
    }

    template<typename VolSqGammaFun>
    Real FiniteDifferencePricer::ValuateImpl(Real price, VolSqGammaFun const& volSqGammaFun)
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
        Real* __restrict next = &mScratch[numPriceSteps];
        for (std::size_t i = 0; i < numPriceSteps; ++i)
            current[i] = Real(0);

        // March from last expiry to next expiry or to 0
        for (std::size_t contractIndex = 0; contractIndex < mContracts.size(); ++contractIndex)
        {
            OptionContract const& contract = *mContracts[contractIndex];

            // Add payoffs
            for (std::uint32_t i = 0; i < numPriceSteps; ++i)
                current[i] += contract.CalculatePayoff(prices[i]) * contract.multiplier;

            // Find next expiry and time to it
            Real const nextExpiry = contractIndex == mContracts.size() - 1 ? Real(0) : mContracts[contractIndex + 1]->expiry;
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
                std::size_t const numPriceSteps_1 = numPriceSteps - 1;
                for (std::size_t i = 1; i < numPriceSteps_1; ++i)
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
                next[numPriceSteps - 1] = Real(2) * next[numPriceSteps - 2] - next[numPriceSteps - 3];

                // next.swap(current);
                std::swap(next, current);
            }
        }

        // Find closest point above current price
        auto it = std::upper_bound(mPrices.begin(), mPrices.end(), price);
        if (it == mPrices.end())
            throw std::runtime_error("Current price not in simulated set");

        auto const index = it - mPrices.begin();

        // If at 0.0, return lowest price value
        if (index == 0)
            return current[0];

        // Interpolate between prices above and below current price
        Real const v0 = current[index - 1];
        Real const v1 = current[index];
        Real const k = (prices[index] - price) / deltaPrice;
        return v0 * k + v1 * (Real(1) - k);
    }

}
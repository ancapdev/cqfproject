#include "fd.hpp"

#include <algorithm>
#include <cassert>
#include <cstdint>

namespace CqfProject
{
#if 0
    namespace
    {
        /// \param volFun Function to generate volatility given gamma. Implements Real operator()(Real gamma) const
        /// \contracts List of options contracts, ordered by descending expiry.
        template<typename VolSqGammaFun>
        Real PricePortfolio_(
            VolSqGammaFun volSqGammaFun,
            Real rate,
            Real currentPrice,
            Real maxPrice,
            Real targetDeltaPrice,
            Real targetDeltaTime,
            const std::vector<OptionContract>& contracts)
        {
            // TODO: Check stability conditions
            //       Could automatically select deltaPrice and deltaTime to meet stability conditions, or pass in error tolerance

            typedef std::vector<Real> Column;
            
            std::size_t const priceSteps = static_cast<std::size_t>(maxPrice / targetDeltaPrice) + 1;
            Real const deltaPrice = maxPrice / (priceSteps - 1);
            Real const deltaPriceSq = deltaPrice * deltaPrice;
            Real const invTwoDeltaPrice = Real(0.5) / deltaPrice;
            Real const invDeltaPriceSq = Real(1) / deltaPriceSq;

            // Initial state
            Column current_(priceSteps, Real(0));
            Real* __restrict current = &current_[0];
            Column next_(priceSteps, Real(0));
            Real* __restrict next = &next_[0];
            
            // Cache prices
            Column prices_(priceSteps, Real(0));
            Real* __restrict prices = &prices_[0];
            for (std::uint32_t i = 0; i < priceSteps; ++i)
                prices[i] = i * deltaPrice;




            // March from last expiry to next expiry or to 0
            for (std::size_t contractIndex = 0; contractIndex < contracts.size(); ++contractIndex)
            {
                OptionContract const& contract = contracts[contractIndex];

                // Add payoffs
                for (std::uint32_t i = 0; i < priceSteps; ++i)
                    current[i] += contract.payoff(prices[i]);

                // Find next expiry and time to it
                Real const nextExpiry = contractIndex == contracts.size() - 1 ? Real(0) : contracts[contractIndex + 1].expiry;
                Real const timeToNextExpiry = contract.expiry - nextExpiry;

                // If contracts are too close together, assume at same expiry
                // This can break under intentionally bad data, but shouldn't in practice
                if (timeToNextExpiry < targetDeltaTime)
                    continue;

                // Simulate to next expiry or 0
                std::uint32_t const timeSteps = static_cast<std::uint32_t>(timeToNextExpiry / targetDeltaTime) + 1;
                Real const deltaTime = timeToNextExpiry / (timeSteps - 1);

                for (std::uint32_t k = 0; k < timeSteps; ++k)
                {
                    // Main grid
                    std::size_t const priceSteps_1 = priceSteps - 1;
                    for (std::size_t i = 1; i < priceSteps_1; ++i)
                    {
                        Real const price = prices[i];
                        Real const delta = (current[i+1] - current[i-1]) * invTwoDeltaPrice;
                        Real const gamma = (current[i+1] - Real(2) * current[i] + current[i-1]) * invDeltaPriceSq;
                        Real const theta = rate * current[i] - Real(0.5) * volSqGammaFun(gamma) * price * price - rate * price * delta;
                        next[i] = current[i] - deltaTime * theta;
                    }

                    // Boundaries
                    next[0] = (Real(1) - rate * deltaTime) * current[0];
                    // TODO: check priceSteps >= 3
                    next[priceSteps - 1] = Real(2) * next[priceSteps - 2] - next[priceSteps - 3];

                    // next.swap(current);
                    std::swap(next, current);
                }
            }

            // Find closest point above current price
            auto it = std::upper_bound(prices_.begin(), prices_.end(), currentPrice);
            if (it == prices_.end())
                throw std::runtime_error("Current price not in simulated set");

            auto const index = it - prices_.begin();

            // If at 0.0, return lowest price value
            if (index == 0)
                return current[0];

            // Interpolate between prices above and below current price
            Real const v0 = current[index - 1];
            Real const v1 = current[index];
            Real const k = (prices[index] - currentPrice) / deltaPrice;
            return v0 * k + v1 * (Real(1) - k);
        }
    }

    std::tuple<Real, Real> PricePortfolio(
        Real minVol,
        Real maxVol,
        Real rate,
        Real currentPrice,
        Real maxPrice,
        Real targetDeltaPrice,
        Real targetDeltaTime,
        std::vector<OptionContract> const& contracts)
    {
        return std::make_tuple(
            PricePortfolio(minVol, maxVol, rate, currentPrice, maxPrice, targetDeltaPrice, targetDeltaTime, Side::BID, contracts),
            PricePortfolio(minVol, maxVol, rate, currentPrice, maxPrice, targetDeltaPrice, targetDeltaTime, Side::ASK, contracts));
    }

    Real PricePortfolio(
        Real minVol,
        Real maxVol,
        Real rate,
        Real currentPrice,
        Real maxPrice,
        Real targetDeltaPrice,
        Real targetDeltaTime,
        Side side,
        std::vector<OptionContract> const& contracts)
    {
        // Check that contracts are sorted by descending expiry
        assert(std::is_sorted(contracts.begin(), contracts.end(), [](OptionContract const& a, OptionContract const& b) { return a.expiry > b.expiry; }));

        // TODO: Remove and do stability properly (based on target error)
        std::uint32_t const priceSteps = static_cast<std::uint32_t>(maxPrice / targetDeltaPrice) + 1;
        targetDeltaTime = Real(0.9) / (priceSteps * priceSteps * maxVol * maxVol);
        
        Real const minVolSq = minVol * minVol;
        Real const maxVolSq = maxVol * maxVol;

        if (side == Side::BID)
        {
            // Minimum portfolio value
            return PricePortfolio_(
                // [=] (Real gamma) { return gamma > 0.0 ? minVolSq * gamma : maxVolSq * gamma; },
                [=] (Real gamma) -> Real
                {
                    // NOTE: Rather than select on gamma > 0 directly, use a min/max like construct which is vectorizable
                    //       Equivalent would be 
                    //       return gamma > 0.0 ? minVolSq * gamma : maxVolSq * gamma;
                    Real const g1 = gamma * minVolSq;
                    Real const g2 = gamma * maxVolSq;
                    return g1 < g2 ? g1 : g2;
                },
                rate, currentPrice, maxPrice, targetDeltaPrice, targetDeltaTime, contracts);
        }
        else
        {
            // Maximum portfolio value
            return PricePortfolio_(
                [=] (Real gamma) -> Real
                {
                    Real const g1 = gamma * minVolSq;
                    Real const g2 = gamma * maxVolSq;
                    return g1 > g2 ? g1 : g2;
                },
                rate, currentPrice, maxPrice, targetDeltaPrice, targetDeltaTime, contracts);
        }
    }
#endif


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
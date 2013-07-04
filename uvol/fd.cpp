#include "fd.hpp"

#include <algorithm>
#include <cstdint>

namespace CqfProject
{
    namespace
    {
        /// \param volFun Function to generate volatility given gamma. Implements double operator()(double gamma) const
        /// \contracts List of options contracts, ordered by descending expiry.
        template<typename VolSqFun>
        double PricePortfolio_(
            VolSqFun volSqFun,
            double rate,
            double currentPrice,
            double maxPrice,
            double targetDeltaPrice,
            double targetDeltaTime,
            const std::vector<OptionContract>& contracts)
        {
            // TODO: Check stability conditions
            //       Could automatically select deltaPrice and deltaTime to meet stability conditions, or pass in error tolerance

            typedef std::vector<double> Column;

            std::uint32_t const priceSteps = static_cast<std::uint32_t>(maxPrice / targetDeltaPrice) + 1;
            double const deltaPrice = maxPrice / (priceSteps - 1);
            double const deltaPriceSq = deltaPrice * deltaPrice;

            // Initial state
            Column current(priceSteps, 0.0);
            Column next(priceSteps, 0.0);

            // Cache prices
            Column prices(priceSteps, 0.0);
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
                double const nextExpiry = contractIndex == contracts.size() - 1 ? 0.0 : contracts[contractIndex + 1].expiry;
                double const timeToNextExpiry = contract.expiry - nextExpiry;

                // If contracts are too close together, assume at same expiry
                // This can break under intentionally bad data, but shouldn't in practice
                if (timeToNextExpiry < targetDeltaTime)
                    continue;

                // Simulate to next expiry or 0
                std::uint32_t const timeSteps = static_cast<std::uint32_t>(timeToNextExpiry / targetDeltaTime) + 1;
                double const deltaTime = timeToNextExpiry / (timeSteps - 1);

                for (std::uint32_t k = 0; k < timeSteps; ++k)
                {
                    // Main grid
                    for (std::uint32_t i = 1; i < (priceSteps - 1); ++i)
                    {
                        double const price = prices[i];
                        double const delta = (current[i+1] - current[i-1]) / (2.0 * deltaPrice);
                        double const gamma = (current[i+1] - 2.0 * current[i] + current[i-1]) / deltaPriceSq;
                        double const volSq = volSqFun(gamma);
                        double const theta = rate * current[i] - 0.5 * volSq * price * price * gamma - rate * price * delta;
                        next[i] = current[i] - deltaTime * theta;
                    }

                    // Boundaries
                    next[0] = (1.0 - rate * deltaTime) * current[0];
                    // TODO: check priceSteps >= 3
                    next[priceSteps - 1] = 2.0 * next[priceSteps - 2] - next[priceSteps - 3];

                    next.swap(current);
                }
            }

            // Find closest point above current price
            auto it = std::upper_bound(prices.begin(), prices.end(), currentPrice);
            if (it == prices.end())
                throw std::runtime_error("Current price not in simulated set");

            auto const index = it - prices.begin();

            // If at 0.0, return lowest price value
            if (index == 0)
                return current[0];

            // Interpolate between prices above and below current price
            double const v0 = current[index - 1];
            double const v1 = current[index];
            double const k = (prices[index] - currentPrice) / deltaPrice;
            return v0 * k + v1 * (1.0 - k);
        }
    }

    OptionContract OptionContract::Call(double expiry, double strike, double multiplier)
    {
        return OptionContract(expiry, [=] (double price) { return multiplier * std::max(price - strike, 0.0); });
    }

    OptionContract OptionContract::Put(double expiry, double strike, double multiplier)
    {
        return OptionContract(expiry, [=] (double price) { return multiplier * std::max(strike - price, 0.0); });
    }

    OptionContract OptionContract::BinaryCall(double expiry, double strike, double multiplier)
    {
        return OptionContract(expiry, [=] (double price) { return price > strike ? multiplier : 0.0; });
    }

    std::tuple<double, double> PricePortfolio(
        double minVol,
        double maxVol,
        double rate,
        double currentPrice,
        double maxPrice,
        double targetDeltaPrice,
        double targetDeltaTime,
        std::vector<OptionContract> contracts)
    {

        // TODO: Remove and do stability properly (based on target error)
        std::uint32_t const priceSteps = static_cast<std::uint32_t>(maxPrice / targetDeltaPrice) + 1;
        targetDeltaTime = 0.9 / (priceSteps * priceSteps * maxVol * maxVol);


        // Sort contracts by descending expiry
        std::sort(
            contracts.begin(),
            contracts.end(),
            [] (OptionContract const& a, OptionContract const& b) { return a.expiry > b.expiry; });

        double const minVolSq = minVol * minVol;
        double const maxVolSq = maxVol * maxVol;

        double const minValue = PricePortfolio_(
            [=] (double gamma) { return gamma > 0.0 ? minVolSq : maxVolSq; },
            rate, currentPrice, maxPrice, targetDeltaPrice, targetDeltaTime, contracts);

        double const maxValue = PricePortfolio_(
            [=] (double gamma) { return gamma > 0.0 ? maxVolSq : minVolSq; },
            rate, currentPrice, maxPrice, targetDeltaPrice, targetDeltaTime, contracts);

        return std::make_tuple(minValue, maxValue);
    }

    double PricePortfolio(
        double minVol,
        double maxVol,
        double rate,
        double currentPrice,
        double maxPrice,
        double targetDeltaPrice,
        double targetDeltaTime,
        Side side,
        std::vector<OptionContract> contracts)
    {
        // TODO: Remove and do stability properly (based on target error)
        std::uint32_t const priceSteps = static_cast<std::uint32_t>(maxPrice / targetDeltaPrice) + 1;
        targetDeltaTime = 0.9 / (priceSteps * priceSteps * maxVol * maxVol);

        // Sort contracts by descending expiry
        std::sort(
            contracts.begin(),
            contracts.end(),
            [] (OptionContract const& a, OptionContract const& b) { return a.expiry > b.expiry; });

        double const minVolSq = minVol * minVol;
        double const maxVolSq = maxVol * maxVol;

        if (side == Side::BID)
        {
            // Minimum portfolio value
            return PricePortfolio_(
                [=] (double gamma) { return gamma > 0.0 ? minVolSq : maxVolSq; },
                rate, currentPrice, maxPrice, targetDeltaPrice, targetDeltaTime, contracts);
        }
        else
        {
            // Maximum portfolio value
            return PricePortfolio_(
                [=] (double gamma) { return gamma > 0.0 ? maxVolSq : minVolSq; },
                rate, currentPrice, maxPrice, targetDeltaPrice, targetDeltaTime, contracts);
        }
    }
}
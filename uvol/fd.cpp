#include "fd.hpp"

#include <algorithm>
#include <cstdint>

namespace CqfProject
{
    std::tuple<double, double> PricePortfolio(
        double minVol,
        double maxVol,
        double rate,
        double maxPrice,
        double targetDeltaPrice,
        double targetDeltaTime,
        std::vector<OptionContract> contracts)
    {
        typedef std::vector<double> Column;

        // TODO: Check stability conditions
        //       Could automatically select deltaPrice and deltaTime to meet stability conditions, or pass in error tolerance


        std::uint32_t const priceSteps = static_cast<std::uint32_t>(maxPrice / targetDeltaPrice) + 1;
        double const deltaPrice = maxPrice / priceSteps;


        // Sort contracts by descending expiry
        std::sort(
            contracts.begin(),
            contracts.end(),
            [] (OptionContract const& a, OptionContract const& b) { return a.expiry > b.expiry; });


        // March from last expiry to next expiry or to 0
        Column current(priceSteps, 0.0);
        Column next(priceSteps, 0.0);

        for (std::size_t contractIndex = 0; contractIndex < contracts.size(); ++contractIndex)
        {
            OptionContract const& contract = contracts[contractIndex];

            // Add payoffs
            for (std::uint32_t i = 0; i < priceSteps; ++i)
                current[i] += contract.payoff(i * deltaPrice);

            // Find next expiry and time to it
            double const nextExpiry = contractIndex == contracts.size() - 1 ? 0.0 : contracts[contractIndex + 1].expiry;
            double const timeToNextExpiry = contract.expiry - nextExpiry;

            // If contracts are too close together, assume at same expiry
            // This can break under intentionally bad data, but shouldn't in practice
            if (timeToNextExpiry < targetDeltaTime)
                continue;

            // Simulate to next expiry or 0
            std::uint32_t const timeSteps = static_cast<std::uint32_t>(timeToNextExpiry / targetDeltaTime) + 1;
            double const deltaTime = timeToNextExpiry / timeSteps;

            for (std::uint32_t i = 0; i < timeSteps; ++i)
            {
                for (std::uint32_t k = 0; k < priceSteps; ++k)
                {


                }
            }
        }

        return std::make_tuple(0.0, 0.0);
    }
}
#include "blackScholes.hpp"
#include "finiteDifferencePricer.hpp"
#include "stopwatch.hpp"

#include <boost/noncopyable.hpp>

#include <cstdint>
#include <chrono>
#include <iostream>

using namespace CqfProject;

Real const rate = 0.05;
Real const price = 100.0;
Real const minVol = 0.10;
Real const maxVol = 0.30;
Real const timeToExpiry = 1.0;

int const BENCHMARK_REPS = 10;


// Very crude benchmarking routine. No statistical analysis, simply mean run time.
void Benchmark(std::size_t minSteps = 20, std::size_t maxSteps = 300, std::size_t stepsStep = 10)
{
    Stopwatch stopwatch;

    for (std::size_t steps = minSteps; steps <= maxSteps; steps += stepsStep)
    {
        // TODO: consider including construction time
        FiniteDifferencePricer pricer(
            minVol,
            maxVol,
            rate,
            price * Real(2),
            steps);

        pricer.AddContract(OptionContract(OptionType::CALL, timeToExpiry, price, 1.0));

        stopwatch.Start();
        for (int i = 0; i < BENCHMARK_REPS; ++i)
            pricer.Valuate(price, Side::BID);
        stopwatch.Stop();
        
        double const nsTotal = static_cast<double>(stopwatch.GetElapsedNanoseconds());
        double const usPerValuation = (nsTotal / 1000.0) / BENCHMARK_REPS;
        std::cout << steps << " steps: " << usPerValuation << "us/valuation" << std::endl;
    }    
}

// Check error from Black Scholes is within tolerance
int TestCorrectness(std::size_t numPriceSteps = 200, Real relTolerance = 0.1, Real minValue = 0.001)
{
    int errorCount = 0;

    OptionContract::Type const contractTypes[4] =
        {
            OptionContract::Type::CALL,
            OptionContract::Type::PUT,
            OptionContract::Type::BINARY_CALL,
            OptionContract::Type::BINARY_PUT
        };

    for (auto typeIt = std::begin(contractTypes); typeIt != std::end(contractTypes); ++typeIt)
    {
        for (Real strike = 0.5 * price; strike < 1.51 * price; strike += price / 100.0)
        {
            // For contracts where gamma can change sign over time,
            // we can't compare our model with Black Scholes, so use constant volatility
            bool alwaysPositiveGamma =
                *typeIt == OptionContract::Type::CALL ||
                *typeIt == OptionContract::Type::PUT;

            FiniteDifferencePricer pricer(
                alwaysPositiveGamma ? minVol : maxVol,
                maxVol,
                rate,
                price * Real(2),
                numPriceSteps);

            pricer.AddContract(OptionContract(*typeIt, timeToExpiry, strike, 1.0));

            Real const fdBid = pricer.Valuate(price, Side::BID);
            Real const fdAsk = pricer.Valuate(price, Side::ASK);
            Real const bsBid = BlackScholesOption(*typeIt, alwaysPositiveGamma ? minVol : maxVol, rate, timeToExpiry, price, strike);
            Real const bsAsk = BlackScholesOption(*typeIt, maxVol, rate, timeToExpiry, price, strike);

            if (bsBid > minValue && std::abs((fdBid - bsBid) / bsBid) > relTolerance)
            {
                std::cout << "Correctness error. Type=" << *typeIt << ", strike=" << strike << ", fdBid=" << fdBid << ", bsBid=" << bsBid << std::endl;
                errorCount++;
            }
                
            if (bsAsk > minValue && std::abs((fdAsk - bsAsk) / bsAsk) > relTolerance)
            {
                std::cout << "Correctness error. Type=" << *typeIt << ", strike=" << strike << ", fdAsk=" << fdAsk << ", bsAsk=" << bsAsk << std::endl; 
                errorCount++;
            }
        }
    }

    return errorCount;
}

// Test error reduces with increased grid resolution
int TestConvergence(std::size_t minSteps = 20, std::size_t maxSteps = 300, std::size_t stepsStep = 10)
{
    int errorCount = 0;
    Real lastBidError = 1e6;
    Real lastAskError = 1e6;

    for (Real strike = 0.5 * price; strike < 1.51 * price; strike += price / 20.0)
    {
        for (std::size_t steps = minSteps; steps <= maxSteps; steps += stepsStep)
        {
            FiniteDifferencePricer pricer(
                minVol,
                maxVol,
                rate,
                price * Real(2),
                steps);

            pricer.AddContract(OptionContract(OptionType::CALL, timeToExpiry, strike, 1.0));

            Real const fdBid = pricer.Valuate(price, Side::BID);
            Real const fdAsk = pricer.Valuate(price, Side::ASK);
            Real const bsBid = BlackScholesOption(OptionType::CALL, minVol, rate, timeToExpiry, price, strike);
            Real const bsAsk = BlackScholesOption(OptionType::CALL, maxVol, rate, timeToExpiry, price, strike);

            Real const bidError = std::abs(bsBid - fdBid);
            Real const askError = std::abs(bsBid - fdBid);

            if (bidError > lastBidError)
            {
                std::cout << "Bid convergence error. Strike=" << strike << ", Error=" << bidError << ", LastError=" << lastBidError << ", Steps=" << steps << std::endl;
                errorCount++;
            }

            if (askError > lastAskError)
            {
                std::cout << "Ask convergence error. Strike=" << strike << ", Error=" << askError << ", LastError=" << lastAskError << ", Steps=" << steps << std::endl;
                errorCount++;
            }
        }
    }

    return errorCount;
}

int main()
{
    using namespace CqfProject;

    std::cout << "Testing correctness" << std::endl;
    int c1 = TestCorrectness();
    if (c1 == 0)
        std::cout << "Correctness tests passed!" << std::endl;
    else
        std::cout << "Correctness tests failed! " << c1 << " errors" << std::endl;

    std::cout << "Testing convergence" << std::endl;
    int c2 = TestConvergence();
    if (c2 == 0)
        std::cout << "Convergence tests passed!" << std::endl;
    else
        std::cout << "Convergence tests failed! " << c2 << " errors" << std::endl;


    std::cout << "Running benchmarks" << std::endl;
    Benchmark();

    return 0;
}

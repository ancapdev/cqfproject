#include "fd.hpp"

#include "nlopt.hpp"

#include <boost/math/distributions/normal.hpp>

#include <cstdint>
#include <chrono>
#include <iostream>

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


    Real const rate = 0.05;
    Real const price = 100.0;
    Real const strike = 100.0;
    Real const overhedgeStrike = 90.0;
    Real const hedgeQty = Real(1.0) / (strike - overhedgeStrike);
    Real const minVol = 0.10;
    Real const maxVol = 0.30;
    Real const impliedVol = 0.20;
    Real const timeToExpiry = 1.0;

#if 0
    Real PriceHedgedBinaryBid(Real x0, Real x1)
    {
        std::vector<OptionContract> contracts;
        contracts.push_back(OptionContract::BinaryCall(timeToExpiry, strike, 1.0));
        contracts.push_back(OptionContract::Call(timeToExpiry, overhedgeStrike, x0));
        contracts.push_back(OptionContract::Call(timeToExpiry, strike, x1));

        auto const prices1  = BlackScholesPutCall(impliedVol, rate, timeToExpiry, price, overhedgeStrike);
        auto const prices2 = BlackScholesPutCall(impliedVol, rate, timeToExpiry, price, strike);
        Real const hedgeCost = prices1.call * x0 + prices2.call * x1;

        return PricePortfolio(
            minVol,
            maxVol,
            rate,
            price,
            price * Real(2),
            Real(1),
            Real(0.01),
            Side::BID,
            contracts) - hedgeCost;
    }

    Real PriceBinaryBid()
    {
        std::vector<OptionContract> contracts;
        contracts.push_back(OptionContract::BinaryCall(timeToExpiry, strike, 1.0));

        return PricePortfolio(
            minVol,
            maxVol,
            rate,
            price,
            price * Real(2),
            Real(1),
            Real(0.01),
            Side::BID,
            contracts);
    }
#endif

    class Stopwatch
    {
    public:
        void Start()
        {
            mStart = std::chrono::high_resolution_clock::now();
        }

        void Stop()
        {
            mStop = std::chrono::high_resolution_clock::now();
        }

        std::int64_t GetElapsedNanoseconds() const
        {
            return std::chrono::duration_cast<std::chrono::nanoseconds>(mStop - mStart).count();
        }

        std::int64_t GetElapsedMicroseconds() const
        {
            return std::chrono::duration_cast<std::chrono::microseconds>(mStop - mStart).count();
        }

        std::int64_t GetElapsedMilliseconds() const
        {
            return std::chrono::duration_cast<std::chrono::milliseconds>(mStop - mStart).count();
        }

    private:
        typedef std::chrono::high_resolution_clock::time_point TimePoint;

        TimePoint mStart;
        TimePoint mStop;
    };
}

int main()
{
    // NOTE: http://www.deltaquants.com/overhedging.html

    using namespace CqfProject;

    Stopwatch stopwatch;
    {
        BinaryCall const binCall(timeToExpiry, strike, 1.0);
        Call const hedge1(timeToExpiry, overhedgeStrike, -hedgeQty);
        Call const hedge2(timeToExpiry, strike, hedgeQty);

        FiniteDifferencePricer pricer(
            minVol,
            maxVol,
            rate,
            price * Real(2),
            201); // TODO: Adjust this after fixing pricer logic

        pricer.AddContract(&binCall);
        
        stopwatch.Start();
        Real const bid = pricer.Valuate(price, Side::BID);
        Real const ask = pricer.Valuate(price, Side::ASK);
        stopwatch.Stop();

        std::cout << "Duration: " << stopwatch.GetElapsedMilliseconds() << "ms" << std::endl;
        std::cout << "Unhedged bid: " << bid << std::endl;
        std::cout << "Unhedged ask: " << ask << std::endl;

        pricer.AddContract(&hedge1);
        pricer.AddContract(&hedge2);

        stopwatch.Start();
        Real const hedgedBid = pricer.Valuate(price, Side::BID);
        Real const hedgedAsk = pricer.Valuate(price, Side::ASK);
        stopwatch.Stop();

        std::cout << "Duration: " << stopwatch.GetElapsedMilliseconds() << "ms" << std::endl;
        std::cout << "Portfolio bid: " << hedgedBid  << std::endl;
        std::cout << "Portfolio ask: " << hedgedAsk << std::endl;

        auto const prices1  = BlackScholesPutCall(impliedVol, rate, timeToExpiry, price, overhedgeStrike);
        auto const prices2 = BlackScholesPutCall(impliedVol, rate, timeToExpiry, price, strike);
        Real const callSpreadPrice = (prices1.call - prices2.call) * hedgeQty;
        std::cout << "Call spread price: " << callSpreadPrice << std::endl;

        std::cout << "Binary bid: " << hedgedBid + callSpreadPrice << std::endl;
        std::cout << "Binary ask: " << hedgedAsk + callSpreadPrice << std::endl;
    }


#if 0
    nlopt::opt optimizer(nlopt::GN_DIRECT, 2);
    
    FiniteDifferencePricer hedgedBinaryPricer(
        minVol,
        maxVol,
        rate,
        price * Real(2),
        201); // TODO: Adjust this after fixing pricer logic


    const BinaryCall binCall(timeToExpiry, strike, 1.0);
    const Call hedge1(timeToExpiry, overhedgeStrike, x0);
    const Call hedge2(timeToExpiry, strike, x1);

    hedgedBinaryPricer.AddContract(&binCall);
    hedgedBinaryPricer.AddContract(&hedge1);
    hedgedBinaryPricer.AddContract(&hedge2);

    auto objectivFunc = [] (std::vector<double> const& x, std::vector<double>&, void* pricer_) -> double
    {
        auto& pricer = *reinterpret_cast<FiniteDifferencePricer*>(pricer_);

        auto const prices1  = BlackScholesPutCall(impliedVol, rate, timeToExpiry, price, overhedgeStrike);
        auto const prices2 = BlackScholesPutCall(impliedVol, rate, timeToExpiry, price, strike);
        Real const hedgeCost = prices1.call * x0 + prices2.call * x1;

        return pricer.Valuate(price, Side::BID) - hedgeCost;
        /*
        Real const value = PriceHedgedBinaryBid((Real)x[0], (Real)x[1]);
        return (double)value;
        */
    };

    // optimizer.set_min_objective(objectivFunc, nullptr);
    optimizer.set_max_objective(objectivFunc, nullptr);
    optimizer.set_lower_bounds(std::vector<double>(2, -1.0));
    optimizer.set_upper_bounds(std::vector<double>(2, 1.0));
    optimizer.set_maxeval(1000);


    std::vector<double> x(2, 0.0);
    double optimizedValue = 0.0;
    try
    {
        stopwatch.Start();
        nlopt::result result = optimizer.optimize(x, optimizedValue);
        stopwatch.Stop();
        std::cout << "Duration: " << stopwatch.GetElapsedMilliseconds() << "ms" << std::endl;
        std::cout << "Overhedge Qty: " << x[0] << std::endl;
        std::cout << "Hedge Qty:     " << x[1] << std::endl;
        std::cout << "Result:        " << result << std::endl;

        Real const bid = PriceBinaryBid();
        Real const hedgedBid = PriceHedgedBinaryBid((Real)x[0], (Real)x[1]);

        auto const prices1  = BlackScholesPutCall(impliedVol, rate, timeToExpiry, price, overhedgeStrike);
        auto const prices2 = BlackScholesPutCall(impliedVol, rate, timeToExpiry, price, strike);
        Real const hedgeCost = prices1.call * (Real)x[0] + prices2.call * (Real)x[1];

        std::cout << "Bid:           " << bid << std::endl;
        std::cout << "Hedged bid:    " << hedgedBid << std::endl;
        std::cout << "hedge1 price:  " << prices1.call << std::endl;
        std::cout << "hedge2 price:  " << prices2.call << std::endl;
        std::cout << "hedge1:        " << prices1.call * x[0] << std::endl;
        std::cout << "hedge2:        " << prices2.call * x[1] << std::endl;
        std::cout << "hedge:         " << hedgeCost << std::endl;
    }
    catch (std::exception& e)
    {
        std::cout << "Error: " << e.what() << std::endl;
    }
#endif

    return 0;
}
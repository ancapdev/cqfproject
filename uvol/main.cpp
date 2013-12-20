#include "blackScholes.hpp"
#include "finiteDifferencePricer.hpp"
#include "stopwatch.hpp"

#include "nlopt.hpp"

#include <boost/noncopyable.hpp>

#include <cstdint>
#include <chrono>
#include <iostream>

namespace CqfProject
{
    Real const rate = 0.05;
    Real const price = 100.0;
    Real const strike = 100.0;
    Real const overhedgeStrike = 90.0;
    Real const hedgeQty = Real(1.0) / (strike - overhedgeStrike);
    Real const minVol = 0.10;
    Real const maxVol = 0.30;
    Real const impliedVol = 0.20;
    Real const timeToExpiry = 1.0;
}

int main()
{
    // NOTE: http://www.deltaquants.com/overhedging.html

    using namespace CqfProject;

    Stopwatch stopwatch;
    {
        OptionContract const binCall(OptionContract::Type::BINARY_CALL, timeToExpiry, strike, 1.0);
        OptionContract const hedge1(OptionContract::Type::CALL, timeToExpiry, overhedgeStrike, -hedgeQty);
        OptionContract const hedge2(OptionContract::Type::CALL, timeToExpiry, strike, hedgeQty);

        FiniteDifferencePricer pricer(
            minVol,
            maxVol,
            rate,
            price * Real(2),
            201); // TODO: Adjust this after fixing pricer logic

        pricer.AddContract(binCall);
        
        stopwatch.Start();
        Real const bid = pricer.Valuate(price, Side::BID);
        Real const ask = pricer.Valuate(price, Side::ASK);
        stopwatch.Stop();

        std::cout << "Duration: " << stopwatch.GetElapsedMilliseconds() << "ms" << std::endl;
        std::cout << "Unhedged bid: " << bid << std::endl;
        std::cout << "Unhedged ask: " << ask << std::endl;

        pricer.AddContract(hedge1);
        pricer.AddContract(hedge2);

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


    nlopt::opt optimizer(nlopt::GN_DIRECT, 2);
    
    class HedgedPricer : boost::noncopyable
    {
    public:
        HedgedPricer()
            : mBinCall(OptionContract::Type::BINARY_CALL, timeToExpiry, strike, Real(1))
            , mHedge1(OptionContract::Type::CALL, timeToExpiry, overhedgeStrike, Real(0))
            , mHedge2(OptionContract::Type::CALL, timeToExpiry, strike, Real(0))
            , mFdPricer(minVol, maxVol, rate, price * Real(2), 201) // TOOD: Adjust price steps after fixing pricer logic
            , mHedge1Price(BlackScholesPutCall(impliedVol, rate, timeToExpiry, price, overhedgeStrike).call)
            , mHedge2Price(BlackScholesPutCall(impliedVol, rate, timeToExpiry, price, strike).call)
        {
            mFdPricer.AddContract(mBinCall);
            mFdPricer.AddContract(mHedge1);
            mFdPricer.AddContract(mHedge2);
        }

        static double ObjectiveFunc(std::vector<double> const& x, std::vector<double>&, void* this_)
        {
            return reinterpret_cast<HedgedPricer*>(this_)->Evaluate((Real)x[0], (Real)x[1]);
        }

        Real Evaluate(Real const x0, Real const x1)
        {
            Real const hedgeCost = mHedge1Price * x0 + mHedge2Price * x1;
            mHedge1.multiplier = x0;
            mHedge2.multiplier = x1;
            return mFdPricer.Valuate(price, Side::BID) - hedgeCost;
        }

    private:
        OptionContract mBinCall;
        OptionContract mHedge1;
        OptionContract mHedge2;
        FiniteDifferencePricer mFdPricer;
        Real const mHedge1Price;
        Real const mHedge2Price;
    };

    HedgedPricer hedgedPricer;

    // optimizer.set_min_objective(objectivFunc, nullptr);
    optimizer.set_max_objective(&HedgedPricer::ObjectiveFunc, &hedgedPricer);
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

        // Real const bid = PriceBinaryBid();
        Real const hedgedBid = hedgedPricer.Evaluate((Real)x[0], (Real)x[1]);

        auto const prices1  = BlackScholesPutCall(impliedVol, rate, timeToExpiry, price, overhedgeStrike);
        auto const prices2 = BlackScholesPutCall(impliedVol, rate, timeToExpiry, price, strike);
        Real const hedgeCost = prices1.call * (Real)x[0] + prices2.call * (Real)x[1];

        // std::cout << "Bid:           " << bid << std::endl;
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

    return 0;
}
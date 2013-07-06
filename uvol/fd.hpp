#ifndef UVOL_FD_HPP
#define UVOL_FD_HPP

#include <cstdint>
#include <functional>
#include <tuple>
#include <vector>

namespace CqfProject
{
    typedef double Real;

    struct OptionContract
    {
        typedef std::function<Real (Real price)> PayoffFunction;

        OptionContract(Real expiry, PayoffFunction const& payoff)
            : expiry(expiry)
            , payoff(payoff)
        {}

        static OptionContract Call(Real expiry, Real strike, Real multiplier);
        static OptionContract Put(Real expiry, Real strike, Real multiplier);
        static OptionContract BinaryCall(Real expiry, Real strike, Real multiplier);

        Real expiry;
        PayoffFunction payoff;
    };
    
    enum class Side
    {
        BID,
        ASK
    };

    std::tuple<Real, Real> PricePortfolio(
        Real minVol,
        Real maxVol,
        Real rate,
        Real currentPrice,
        Real maxPrice,
        Real targetDeltaPrice,
        Real targetDeltaTime,
        std::vector<OptionContract> const& contracts);

    Real PricePortfolio(
        Real minVol,
        Real maxVol,
        Real rate,
        Real currentPrice,
        Real maxPrice,
        Real targetDeltaPrice,
        Real targetDeltaTime,
        Side side,
        std::vector<OptionContract> const& contracts);

    class FiniteDifferencePricer
    {
    public:
        FiniteDifferencePricer(
            Real minVol,
            Real maxVol,
            Real rate,
            Real maxPrice,
            std::size_t numPriceSteps);

        void AddContract(OptionContract const& contract);

        Real Valuate(Real price, Side side);

    private:
        template<typename VolSqGammaFun>
        Real ValuateImpl(Real price, VolSqGammaFun const& volSqGammaFun);

        //
        // User provided parameters
        // 
        Real mMinVol;
        Real mMaxVol;
        Real mRate;
        Real mMaxPrice;
        // TODO: Fix so grid size is numPriceSteps + 1
        std::size_t mNumPriceSteps;
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

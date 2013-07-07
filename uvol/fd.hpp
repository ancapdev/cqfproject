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
        OptionContract(Real expiry, Real multiplier)
            : expiry(expiry)
            , multiplier(multiplier)
        {}

        virtual Real CalculatePayoff(Real price) const = 0;

        Real expiry;
        Real multiplier;
    };

    template<typename PayoffFunc>
    struct SimpleContract : OptionContract, PayoffFunc
    {
        SimpleContract(Real expiry, Real strike, Real multiplier) : OptionContract(expiry, multiplier), strike(strike) {}

        virtual Real CalculatePayoff(Real price) const override
        {
            return (*this)(strike, price);
        }

        Real strike;
    };

    struct CallPayoff       { Real operator () (Real strike, Real price) const { return std::max(price - strike, Real(0)); } };
    struct PutPayoff        { Real operator () (Real strike, Real price) const { return std::max(strike - price, Real(0)); } };
    struct BinarYCallPayoff { Real operator () (Real strike, Real price) const { return price > strike ? 1.0 : 0.0; } };

    typedef SimpleContract<CallPayoff> Call;
    typedef SimpleContract<PutPayoff> Put;
    typedef SimpleContract<BinarYCallPayoff> BinaryCall;

    enum class Side
    {
        BID,
        ASK
    };

    class FiniteDifferencePricer
    {
    public:
        FiniteDifferencePricer(
            Real minVol,
            Real maxVol,
            Real rate,
            Real maxPrice,
            std::size_t numPriceSteps);

        void AddContract(OptionContract const* contract);

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
        std::vector<OptionContract const*> mContracts;

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

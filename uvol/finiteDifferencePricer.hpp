#ifndef UVOL_FINITE_DIFFERENCE_PRICER_HPP
#define UVOL_FINITE_DIFFERENCE_PRICER_HPP

#include "avx.hpp"
#include "optionContract.hpp"
#include "types.hpp"

#include <algorithm>
#include <cassert>
#include <cstdint>
#include <cstdlib>
#include <memory>
#include <stdexcept>
#include <vector>

namespace CqfProject
{
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
            , mTargetDeltaTime(Real(0.9) / (numPriceSteps * numPriceSteps * maxVol * maxVol))
            , mAllocation(nullptr)
        {
            assert(maxVol >= minVol);

            // Padded price step arrays for
            // price, (alpha, beta, gamma) * 2, scratch * 2
            std::size_t const priceStepChunks = ((numPriceSteps + 1) * sizeof(Real) + 31 + 32) / 32;
            std::size_t const allocRequirement = priceStepChunks * 32 * 9;
            mAllocation = malloc(allocRequirement + 32);
            void* allignedAllocation = (void*)(((std::uintptr_t)mAllocation + 31u) & ~(std::uintptr_t)31u);

            std::size_t const realPerChunk = 32 / sizeof(Real);
            std::size_t const realPerArray = priceStepChunks * realPerChunk;
            mPrices = (Real*)allignedAllocation;
            mAlpha1 = mPrices + realPerArray;
            mBeta1 = mAlpha1 + realPerArray;
            mGamma1 = mBeta1 + realPerArray;
            mAlpha2 = mGamma1 + realPerArray;
            mBeta2 = mAlpha2 + realPerArray;
            mGamma2 = mBeta2 + realPerArray;
            mScratch1 = mGamma2 + realPerArray;
            mScratch2 = mScratch1 + realPerArray;

            // Pre-calculate prices
            for (std::size_t i = 0; i <= mNumPriceSteps; ++i)
                mPrices[i] = i * mDeltaPrice;
        }

        ~FiniteDifferencePricer()
        {
            std::free(mAllocation);
        }

        void AddContract(OptionContract const& contract)
        {
            mContracts.push_back(contract);
        }

        Real const* BeginPrices() const
        {
            return mPrices;
        }

        Real const* EndPrices() const
        {
            return mPrices + mNumPriceSteps + 1;
        }

        Real Valuate(Real price, Side side)
        {
            return Valuate(price, side, NullOutIt(), 0);
        }

        template<typename OutIt>
        Real Valuate(Real price, Side side, OutIt valuesOut, int detail)
        {
            // Maintain contracts sorted by descending expiry
            auto const expiryGreater = [] (OptionContract const& a, OptionContract const& b) { return a.expiry > b.expiry; };
            if (!std::is_sorted(mContracts.begin(), mContracts.end(), expiryGreater))
                std::sort(mContracts.begin(), mContracts.end(), expiryGreater);

            if (side == Side::BID)
            {
                // Minimum portfolio value
                return ValuateImpl(price, SelectMin(), valuesOut, detail);
            }
            else
            {
                // Maximum portfolio value
                return ValuateImpl(price, SelectMax(), valuesOut, detail);
            }
        }

    private:
        struct SelectMin
        {
            Real operator() (Real a, Real b) const
            {
                return a < b ? a : b;
            }

#if defined (USE_AVX)
            __m256d operator()(__m256d a, __m256d b) const
            {
                return _mm256_min_pd(a, b);
            }
#endif
        };

        struct SelectMax
        {
            Real operator() (Real a, Real b) const
            {
                return a > b ? a : b;
            }

#if defined (USE_AVX)
            __m256d operator()(__m256d a, __m256d b) const
            {
                return _mm256_max_pd(a, b);
            }
#endif
        };

        template<typename MinMaxSelector, typename OutIt>
        Real ValuateImpl(Real price, MinMaxSelector minMaxSelector, OutIt valuesOut, int detail)
        {
            std::size_t numPriceSteps = mNumPriceSteps;
            Real const deltaPrice = mDeltaPrice;
            Real const deltaPriceSq = deltaPrice * deltaPrice;
            Real const halfDeltaPrice = Real(0.5) * deltaPrice;
            Real const invTwoDeltaPrice = Real(0.5) / deltaPrice;
            Real const invDeltaPriceSq = Real(1) / deltaPriceSq;
            Real const minVolSq = mMinVol * mMinVol;
            Real const maxVolSq = mMaxVol * mMaxVol;
            Real const rate = mRate;

            // Cache pointers with alignment and aliasing hint to compiler
            Real const* RESTRICT prices = (Real const*)ASSUME_ALIGNED(mPrices, 32);
            Real* RESTRICT alpha1 = (Real*)ASSUME_ALIGNED(mAlpha1, 32);
            Real* RESTRICT beta1 = (Real*)ASSUME_ALIGNED(mBeta1, 32);
            Real* RESTRICT gamma1 = (Real*)ASSUME_ALIGNED(mGamma1, 32);
            Real* RESTRICT alpha2 = (Real*)ASSUME_ALIGNED(mAlpha2, 32);
            Real* RESTRICT beta2 = (Real*)ASSUME_ALIGNED(mBeta2, 32);
            Real* RESTRICT gamma2 = (Real*)ASSUME_ALIGNED(mGamma2, 32);
            Real* RESTRICT current = (Real*)ASSUME_ALIGNED(mScratch1, 32);
            Real* RESTRICT next = (Real*)ASSUME_ALIGNED(mScratch2, 32);

            // Initial state
            for (std::size_t i = 0; i <= numPriceSteps; ++i)
                current[i] = Real(0);

            // March from last expiry to next expiry or to 0
            for (std::size_t contractIndex = 0; contractIndex < mContracts.size(); ++contractIndex)
            {
                OptionContract const& contract = mContracts[contractIndex];

                // Add payoffs
                for (std::uint32_t i = 0; i <= numPriceSteps; ++i)
                    current[i] += contract.CalculateAveragePayoff(
                        prices[i] - halfDeltaPrice,
                        prices[i] + halfDeltaPrice) * contract.multiplier;

                // Find next expiry and time to it
                Real const nextExpiry = contractIndex == mContracts.size() - 1 ? Real(0) : mContracts[contractIndex + 1].expiry;
                Real const timeToNextExpiry = contract.expiry - nextExpiry;

                // If contracts are too close together, assume at same expiry
                // This can break under intentionally bad data, but shouldn't in practice
                if (timeToNextExpiry < mTargetDeltaTime)
                    continue;

                // Simulate to next expiry or 0
                std::size_t const timeSteps = static_cast<std::size_t>(timeToNextExpiry / mTargetDeltaTime) + 1;
                Real const deltaTime = timeToNextExpiry / timeSteps;

                // Pre-cache coefficients
                for (std::size_t i = 0; i < numPriceSteps; ++i)
                {
                    // Coefficients at i are for calculating at price level i + 1 (for alignment)
                    double const ii = static_cast<double>(i + 1);
                    double const ii2 = ii * ii;

                    alpha1[i] = deltaTime * (0.5 * minVolSq * ii2 - 0.5 * rate * ii);
                    beta1[i] = 1 + deltaTime * (-minVolSq * ii2 - rate);
                    gamma1[i] = deltaTime * (0.5 * minVolSq * ii2 + 0.5 * rate * ii);

                    alpha2[i] = deltaTime * (0.5 * maxVolSq * ii2 - 0.5 * rate * ii);
                    beta2[i] = 1 + deltaTime * (-maxVolSq * ii2 - rate);
                    gamma2[i] = deltaTime * (0.5 * maxVolSq * ii2 + 0.5 * rate * ii);
                }

                for (std::size_t k = 0; k < timeSteps; ++k)
                {
                    // Write out values of entire grid if requested
                    if (detail >= 2)
                        for (std::uint32_t i = 0; i <= numPriceSteps; ++i)
                            *valuesOut++ = current[i];

                    // Main grid
#if defined (USE_AVX)
                    __m256d lower = _mm256_load_pd(current);
                    // Each iteration calculates next[i+1:i+5]
                    // earliest termination: i = numPriceSteps - 1 -> last = [numPriceSteps - 4, numPriceSteps - 1]
                    // latest termination :  i = numPriceSteps + 2 -> last = [numPriceSteps - 1, numPriceSteps + 2]
                    for (std::size_t i = 0; i < (numPriceSteps - 1); i += 4)
                    {
                        __m256d const upper = _mm256_load_pd(current + i + 4);
        
                        // down = lower[0:3]
                        __m256d const current_down = lower;

                        // up = lower[2:3], upper[1:2]
                        __m256d const current_up = _mm256_permute2f128_pd(lower, upper, 1 | (2 << 4));

                        // at = lower[1:3], upper[1]
                        __m256d const t1 = _mm256_permute_pd(lower, 1 | (1 << 2)); // l1, X, l3, X
                        __m256d const current_at = _mm256_unpacklo_pd(t1, current_up); // l1, l2, l3, u1

                        lower = upper;

                        __m256d const a1 = _mm256_load_pd(alpha1 + i);
                        __m256d const b1 = _mm256_load_pd(beta1 + i);
                        __m256d const g1 = _mm256_load_pd(gamma1 + i);
                        __m256d const a2 = _mm256_load_pd(alpha2 + i);
                        __m256d const b2 = _mm256_load_pd(beta2 + i);
                        __m256d const g2 = _mm256_load_pd(gamma2 + i);

                        __m256d const next1 =
                            _mm256_add_pd(
                                _mm256_add_pd(
                                    _mm256_mul_pd(current_down, a1),
                                    _mm256_mul_pd(current_at, b1)),
                                _mm256_mul_pd(current_up, g1));

                        __m256d const next2 =
                            _mm256_add_pd(
                                _mm256_add_pd(
                                    _mm256_mul_pd(current_down, a2),
                                    _mm256_mul_pd(current_at, b2)),
                                _mm256_mul_pd(current_up, g2));

                        _mm256_storeu_pd(next + i + 1, minMaxSelector(next1, next2));
                    }
#else

                    for (std::size_t i = 1; i < numPriceSteps; ++i)
                    {
                        Real const next1 =
                            current[i - 1] * alpha1[i - 1] +
                            current[i] * beta1[i - 1] + 
                            current[i + 1] * gamma1[i - 1];

                        Real const next2 =
                            current[i - 1] * alpha2[i - 1] +
                            current[i] * beta2[i - 1] + 
                            current[i + 1] * gamma2[i - 1];

                        /*
                        Real const price = prices[i];
                        Real const delta = (current[i+1] - current[i-1]) * invTwoDeltaPrice;
                        Real const gamma = (current[i+1] - Real(2) * current[i] + current[i-1]) * invDeltaPriceSq;
                        Real const theta = rate * current[i] - Real(0.5) * volSqGammaFun(gamma) * price * price - rate * price * delta;
                        next[i] = current[i] - deltaTime * theta;
                        */

                        next[i] = minMaxSelector(next1, next2);
                    }
#endif

                    // Boundaries
                    next[0] = (Real(1) - rate * deltaTime) * current[0];
                    // TODO: check numPriceSteps >= 3
                    next[numPriceSteps] = Real(2) * next[numPriceSteps - 1] - next[numPriceSteps - 2];

                    std::swap(next, current);
                }
            }

            // Copy out values
            if (detail >= 1)
                for (std::uint32_t i = 0; i <= numPriceSteps; ++i)
                    *valuesOut++ = current[i];

            // Find closest point above current price
            auto it = std::upper_bound(mPrices, mPrices + mNumPriceSteps + 1, price);
            if (it == mPrices + mNumPriceSteps + 1)
                throw std::runtime_error("Current price not in simulated set");

            auto const index = it - mPrices;

            // If at 0.0, return lowest price value
            if (index == 0)
                return current[0];

            // Interpolate between prices above and below current price
            if (mInterpolation == Interpolation::CUBIC &&
                index > 1u &&
                index < mNumPriceSteps)
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
        // Work space and cache, to avoid repeated allocation and calculations.
        // Uses a single allocation, linearly allocates from this space
        // to set up other work areas (properly aligned and padded for vectorization)
        //
        void* mAllocation;
        Real* mPrices;
        Real* mAlpha1;
        Real* mBeta1;
        Real* mGamma1;
        Real* mAlpha2;
        Real* mBeta2;
        Real* mGamma2;
        Real* mScratch1;
        Real* mScratch2;
    };
}

#endif

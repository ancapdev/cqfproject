#ifndef AVX_HPP
#define AVX_HPP

#if defined (__AVX__)
#   define USE_AVX
#endif

#if defined (USE_AVX)
#   include <immintrin.h>
#endif

#if defined (__GNUC__) && (__GNUC__ > 4 || (__GNUC__ == 4 && __GNUC_MINOR__ >= 7))
#   define ASSUME_ALIGNED(p, a) __builtin_assume_aligned(p, a)
#else
#   define ASSUME_ALIGNED(p, a) (p)
#endif

#define RESTRICT __restrict

#endif

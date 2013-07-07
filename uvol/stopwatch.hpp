#ifndef UVOL_STOPWATCH_HPP
#define UVOL_STOPWATCH_HPP

#include <chrono>

namespace CqfProject
{
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

#endif
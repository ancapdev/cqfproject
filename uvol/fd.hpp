#ifndef UVOL_FD_HPP
#define UVOL_FD_HPP

#include <vector>

namespace CqfProject
{


/*

For multiple expiries, could have a partial evaluation function which returns the whole column of the grid. 
The column could be used as payoffs + payoff of newly expired in next partial
*/

// payoffFunc(price)
// valueFunc(value, price, time)
template<typename ValueFunc, typename PayoffFunc>
double FiniteDifferenceValue(
    double maxPrice,
    double deltaPrice,
    double maxTime,
    double deltaTime,
    double minVol,
    double maxVol,
    double rate,
    ValueFunc valueFunc,
    PayoffFunc payoffFunc)
{
    typedef std::vector<double> Column;

    Column current;

  
    return 0.0;
}

}

#endif

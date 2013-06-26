#ifndef UVOL_FD_HPP
#define UVOL_FD_HPP

namespace CqfProject
{

// Need
// - boundary conditions
// - initial payoffs
// - time step
// - price step
// - max price
// - termination time
template<typename ValueFunc>
double FiniteDifferenceValue(
    double maxPrice,
    double deltaPrice,
    ValueFunc valueFunc,
    PayoffFunc payoffFunc)
{
    typedef std::vector<double> Column;

    Column current;

    for (double price = 0.0; price < maxPrice; price += deltaPrice)
    {
        current.push_back(payoffFunc(price));
    }

}

}

#endif

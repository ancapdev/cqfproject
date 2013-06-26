#include "fd.hpp"

#include <iostream>

int main()
{
    using namespace CqfProject;

    double const value = FiniteDifferenceValue(
        100.0,
        1.0,
        [] {},
        [] (double price) { return price > 50.0 ? 1.0 : 0.0; });

    std::cout << value << std::endl;

    return 0;
}
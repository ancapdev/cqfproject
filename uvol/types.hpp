#ifndef UVOL_TYPES_HPP
#define UVOL_TYPES_HPP

#include <ostream>
#include <stdexcept>

namespace CqfProject
{
    typedef double Real;

    enum class OptionType
    {
        CALL,
        PUT,
        BINARY_CALL,
        BINARY_PUT
    };

    std::ostream& operator << (std::ostream& os, OptionType optionType)
    {
        switch (optionType)
        {
        case OptionType::CALL:
            return os << "call";

        case OptionType::PUT:
            return os << "put";

        case OptionType::BINARY_CALL:
            return os << "bcall";

        case OptionType::BINARY_PUT:
            return os << "bput";

        default:
            throw std::runtime_error("invalid option type");
        }
    }
}

#endif

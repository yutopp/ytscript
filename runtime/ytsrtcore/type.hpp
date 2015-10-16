#pragma once

namespace ytscript
{
    enum struct basic_type : std::uint8_t
    {
        none,

        _val_t_begin,
        e_int32,
        e_double,
        e_null,
        _val_t_end,

        e_string,
        e_label,
        e_function,
        e_object,
    };
}

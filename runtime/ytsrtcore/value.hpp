#pragma once

#include "type.hpp"
#include "object_fwd.hpp"


namespace ytscript
{
    enum struct value_tag : std::uint8_t
    {
        impl_symbol_offset,
        impl_args_num,

        primitive,
        reference,
    };

    union comp_val {
        std::int32_t    val_int32;
        double          val_double;

        std::int32_t    symbol_offset;
        std::int32_t    impl_args_num;

        object::base*   ptr_to_obj;
    };

    inline auto make_comp_val( std::int32_t v )
        -> comp_val
    {
        comp_val u;
        u.val_int32 = v;

        return u;
    }

    inline auto make_comp_val( double v )
        -> comp_val
    {
        comp_val u;
        u.val_double = v;

        return u;
    }

    inline auto make_comp_val( object::base* v )
        -> comp_val
    {
        comp_val u;
        u.ptr_to_obj = v;

        return u;
    }

    struct ytval
    {
        comp_val    value;
        basic_type  type;
        value_tag   tag;
    };

    inline auto is_function( ytval const& val )
        -> bool
    {
        return val.type == basic_type::e_function;
    }

} // namespace ytscript

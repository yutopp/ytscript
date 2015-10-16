#include "stack.hpp"
#include "object.hpp"


namespace ytscript
{
    void stack::push_value( object::base* ptr )
    {
        buffer_[sp_] = ytval {
            make_comp_val( std::move( ptr ) ),
            ptr->get_basic_type(),
            value_tag::reference
        };
        ++sp_;
    }

} // namespace ytscript

#include "context.hpp"

#include "object.hpp"


namespace ytscript
{
    auto context::fetch_value( var_key_type const& key )
        -> boost::optional<ytval&>
    {
        auto const& it = obj_map_.find( key );
        if ( it == obj_map_.end() ) {
            return boost::none;
        }

        return std::get<1>( *it );
    }

    void context::store_value( var_key_type const& key, object::base* ptr )
    {
        obj_map_[key]= ytval {
            make_comp_val( ptr ),
            ptr->get_basic_type(),
            value_tag::reference
        };
    }

    auto context::fetch_label( var_key_type const& key )
        -> object::label*
    {
        auto const& it = label_map_.find( key );
        if ( it == label_map_.end() ) {
            return nullptr;
        }

        return std::get<1>( *it );
    }

    void context::store_label( var_key_type const& key, object::label* ptr )
    {
        assert( ptr != nullptr );
        label_map_[key]= ptr;
    }
}

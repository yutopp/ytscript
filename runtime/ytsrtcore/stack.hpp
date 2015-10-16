#pragma once

#include <array>
#include <type_traits>
#include <cassert>

#include "value.hpp"


namespace ytscript
{
    struct stack
    {
        static constexpr std::size_t StackSize = 256;

    public:
        template<typename T>
        void push_system_value( T&& v, value_tag const& tag )
        {
            buffer_[sp_] = ytval {
                make_comp_val( std::forward<T>( v ) ),
                basic_type::none,
                tag
            };
            ++sp_;
        }

        template<typename T>
        void push_value( T&& v, basic_type const& type )
        {
            assert( basic_type::_val_t_begin < type && type < basic_type::_val_t_end );

            buffer_[sp_] = ytval {
                make_comp_val( std::forward<T>( v ) ),
                type,
                value_tag::primitive
            };
            ++sp_;
        }

        void push_value( object::base* ptr );

        void push_value( ytval const& val )
        {
            buffer_[sp_] = val;
            ++sp_;
        }

        auto pop()
        {
            assert( sp_ > 0 );
            auto v = top();
            --sp_;

            return v;
        }

        void pop_del()
        {
            assert( sp_ > 0 );
            --sp_;
        }

        auto top()
            -> ytval&
        {
            assert( sp_ > 0 );
            auto& s = buffer_[sp_-1];

            return s;
        }

        auto args_num()
        {
            if ( sp_ > 0 ) {
                auto const& p = top();
                if ( p.tag == value_tag::impl_args_num ) {
                    pop_del();
                    return p.value.impl_args_num;

                } else {
                    return 0;
                }

            } else {
                return 0;
            }
        }

    private:
        std::size_t sp_ = 0;
        std::array<ytval, StackSize> buffer_;
    };

}

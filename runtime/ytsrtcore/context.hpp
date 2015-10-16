#pragma once

#include <unordered_map>
#include <string>
#include <memory>

#include <boost/optional.hpp>

#include "module.hpp"
#include "stack.hpp"
#include "object_fwd.hpp"


namespace ytscript
{
    enum class state
    {
        running,
        exit,
    };

    class context
    {
        using var_key_type = char const*;

    public:
        // ownership is managed by this context
        template<typename T, typename ...Args>
        auto allocate_object( Args&&... args )
            -> T*
        {
            // TODO: fix fix fix fix !!!!!!!!
            return new T( std::forward<Args>( args )... );
        }

        template<typename F>
        void register_buildin_func(
            var_key_type const& key,
            F const& f
            )
        {
            auto fp = allocate_object<object::builtin_function<F>>( f );
            store_value( key, fp );
        }

    public:
        auto fetch_value( var_key_type const& key )
            -> boost::optional<ytval&>;

        template<typename T>
        void store_value( var_key_type const& key, T&& v, basic_type const& type )
        {
            assert( basic_type::_val_t_begin < type && type < basic_type::_val_t_end );

            obj_map_[key] = ytval {
                { std::forward<T>( v ) },
                type,
                value_tag::primitive
            };
        }

        void store_value( var_key_type const& key, object::base* ptr );

        template<typename K>
        void store_value( K const& key, ytval const& val )
        {
            obj_map_[key] = val;
        }

    public:
        auto fetch_label( var_key_type const& key )
            -> object::label*;

        void store_label( var_key_type const& key, object::label* ptr );

    public:
        void set_state( state const& s )
        {
            state_ = s;
        }

        auto get_state() const
            -> state const&
        {
            return state_;
        }

    private:
        state state_ = state::running;

    public:
        std::shared_ptr<module> current_module_;
        stack stack_;

    private:
        std::unordered_map<std::string, ytval> obj_map_;
        std::unordered_map<std::string, object::label*> label_map_;
    };

} // namespace ytscript

#pragma once

#include <algorithm>
#include <type_traits>
#include <cassert>
#include <unordered_map>
#include <memory>


#include "context.hpp"
#include "stack.hpp"
#include "object.hpp"
#include "gui_runtime.hpp"


namespace ytscript
{
    template<typename Runtime>
    class engine
    {
        using self_type = engine<Runtime>;
        using this_type = self_type*;

        using module_ptr = std::shared_ptr<module>;
        using intrinsic_func_t = void (engine::*)( context& );

    public:
        engine( char const* const entry_module_name )
            : runtime_( ctx_ )
        {
            mod_ = std::make_shared<module>( entry_module_name );

            ctx_.current_module_ = mod_;

            // init inst table
            std::fill( op_1_.begin(), op_1_.end(), &self_type::abort );
            op_1_[0x01] = &self_type::push;
            op_1_[0xf0] = &self_type::invoke_or_fetch;
            op_1_[0xf1] = &self_type::push_arg_num;

            op_1_[0xf8] = &self_type::def_var;
            op_1_[0xfa] = &self_type::goto_label;

            op_1_[0x10] = &self_type::set_label;
        }

    public:
        void main_loop()
        {
            for(;;) {
                auto const& f = op_1_[take_code<byte_t, 1>()];

                runtime_( ctx_ );
                (this->*f)( ctx_ );

                if ( ctx_.get_state() == state::exit ) {
                    break;
                }
            }
        }

    private:
        template<typename T, std::size_t Size>
        inline auto take_code()
        {
            return ctx_.current_module_->code_section_.take<T, Size>();
        }

        inline auto ref_data_as_text( std::size_t const& offset ) const
        {
            return ctx_.current_module_->data_section_.ref_as_text( offset );
        }

    private:
        inline auto current_ip() const
        {
            // TODO: add module id
            return ctx_.current_module_->code_section_.get_ip();
        }

    private:
        void abort( context& ctx ) {
            std::cout << "invalid instruction" << std::endl;
            assert( false );
        }

        void push( context& ctx ) {
            std::cout << "push" << std::endl;

            std::int32_t val = take_code<std::int32_t, 4>();
            std::cout << "value: " << val << std::endl;

            ctx_.stack_.push_value( val, basic_type::e_int32 );
        }

        void push_arg_num( context& ctx ) {
            std::cout << "push arg num" << std::endl;

            std::int32_t arg0 = take_code<std::int32_t, 4>();
            std::cout << "value: " << arg0 << std::endl;

            ctx_.stack_.push_system_value( arg0, value_tag::impl_args_num );
        }

        void invoke_or_fetch( context& ctx ) {
            std::cout << "invoke_or_fetch" << std::endl;

            // take object name
            std::int32_t const arg0 = take_code<std::int32_t, 4>();
            char const* const name = ref_data_as_text( arg0 );

            std::cout << "->" << name << " / " << arg0 << std::endl;

            auto const& val = ctx_.fetch_value( name );
            if ( val == boost::none ) {
                std::cout << "not found: " << name << std::endl;
                assert( false && "object" );
            }

            //
            if ( is_function( *val ) ) {
                invoke_function( ctx, *val );

            } else {
                ctx_.stack_.push_value( *val );
            }
        }

        void set_label( context& ctx ) {
            std::cout << "set label" << std::endl;

            std::int32_t const arg0 = take_code<std::int32_t, 4>();
            char const* const name = ref_data_as_text( arg0 );

            std::cout << "->" << name << " / " << arg0 << std::endl;

            auto const& val = ctx_.fetch_label( name );
            if ( val ) {
                std::cout << "already defined: " << name << std::endl;
                assert( false && "object" );
            }

            ctx_.store_label(
                name,
                ctx_.allocate_object<object::label>( current_ip() )
                );
        }

        void def_var( context& ctx )
        {
        }

        void goto_label( context& ctx )
        {
            std::cout << "goto" << std::endl;

            std::int32_t const arg0 = take_code<std::int32_t, 4>();
            char const* const name = ref_data_as_text( arg0 );

            std::cout << "->" << name << " / " << arg0 << std::endl;

            auto const& label_object = ctx_.fetch_label( name );
            if ( label_object == nullptr ) {
                std::cout << "label not found: " << name << std::endl;
                assert( false && "object" );
            }

            label_object->execute( ctx );
        }

    private:
        void invoke_function( context& ctx, ytval const& val )
        {
            assert( is_function( val ) );
            assert( val.value.ptr_to_obj != nullptr );

            val.value.ptr_to_obj->execute( ctx );
        }

    private:
        module_ptr mod_;    // TODO: multi module
        context ctx_;

        Runtime runtime_;

        std::array<intrinsic_func_t, 256> op_1_;
    };

} // namespace ytscript

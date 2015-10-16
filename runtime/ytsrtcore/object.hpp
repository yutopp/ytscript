#include "type.hpp"
#include "context.hpp"


namespace ytscript
{
    namespace object
    {
        class base
        {
        public:
            virtual void execute( context& ctx ) =0;
            virtual auto get_basic_type() const -> basic_type =0;
        };


        class label : public base
        {
        public:
            label( std::size_t const& ip )
                : ip_( ip )
            {}

        public:
            void execute( context& ctx ) override
            {
                // ctx.current_module_ = ???
                ctx.current_module_->code_section_.set_ip( ip_ );
            }

            auto get_basic_type() const
                -> basic_type override
            {
                return basic_type::e_label;
            }

        private:
            std::size_t ip_;
        };


        template<typename F>
        class builtin_function : public base
        {
        public:
            builtin_function( F const& f )
                : f_( f )
            {}

        public:
            void execute( context& ctx ) override
            {
                f_( ctx );
            }

            auto get_basic_type() const
                -> basic_type override
            {
                return basic_type::e_function;
            }

        private:
            F f_;
        };

    } // namespace object
} // namespace ytscript

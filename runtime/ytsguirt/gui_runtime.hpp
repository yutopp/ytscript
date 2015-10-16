#include <ytsrtcore/context.hpp>
#include "graphics.hpp"


namespace ytscript
{
    // WIP
    class gui_info
    {
    public:
        gui_info()
        {
            // TODO: fix
            font_ = gctx_.make_font( "/usr/share/fonts/TTF/mplus-1p-black.ttf", 36 );
        }

    public:
        void set_window()
        {
            window_ = gctx_.make_window();
        }

    private:
        graphics::sdl::context gctx_;

    public:
        // TODO: fix
        std::shared_ptr<graphics::sdl::window> window_;
        std::shared_ptr<graphics::sdl::font> font_;
    };


    class graphics_runtime
    {
    public:
        graphics_runtime( context& init_ctx )
        {
            init_ctx.register_buildin_func(
                "make_window",
                [&]( context& ctx ) {
                    make_window( ctx );
                });

            init_ctx.register_buildin_func(
                "begin_render",
                [&]( context& ctx ) {
                    begin_render( ctx );
                });

            init_ctx.register_buildin_func(
                "end_render",
                [&]( context& ctx ) {
                    end_render( ctx );
                });

            init_ctx.register_buildin_func(
                "print_mouse_position",
                [&]( context& ctx ) {
                    print_mouse_position( ctx );
                });

            init_ctx.register_buildin_func(
                "wait",
                [&]( context& ctx ) {
                    wait( ctx );
                });
    }

    public:
        void operator()( context& ctx )
        {
            // TODO: move this loop...
            if ( SDL_PollEvent( &e ) ) {
                switch( e.type ) {
                case SDL_QUIT:
                    ctx.set_state( state::exit );
                    break;

                default:
                    break;
                }
            }
        }

    private:
        void make_window( context& ctx )
        {
            auto const& args_num = ctx.stack_.args_num();
            std::cout << "args num: " << args_num << std::endl;
            assert( args_num == 0 );

            ginfo_.set_window();
        }

        void begin_render( context& ctx )
        {
            auto const& args_num = ctx.stack_.args_num();
            std::cout << "args num: " << args_num << std::endl;
            assert( args_num == 0 );

            if ( ginfo_.window_ ) {
                ginfo_.window_->begin_render();
            }
        }

        void end_render( context& ctx )
        {
            auto const& args_num = ctx.stack_.args_num();
            std::cout << "args num: " << args_num << std::endl;
            assert( args_num == 0 );

            if ( ginfo_.window_ ) {
                ginfo_.window_->end_render();
            }
        }

        void wait( context& ctx )
        {
            auto const& args_num = ctx.stack_.args_num();
            std::cout << "args num: " << args_num << std::endl;
            assert( args_num == 1 );

            auto arg0 = ctx.stack_.pop();
            assert( arg0.type == basic_type::e_int32 && "invalid type" );
            std::cout << "sec: " << arg0.value.val_int32 << std::endl;

            // TODO: fix
            ::SDL_Delay( arg0.value.val_int32 );
        }

        // ha??
        void print_mouse_position( context& ctx )
        {
            auto const& args_num = ctx.stack_.args_num();
            std::cout << "args num: " << args_num << std::endl;
            assert( args_num == 0 );

            if ( ginfo_.window_ ) {
                int x, y;
                ::SDL_GetMouseState( &x, &y );

                std::stringstream ss;
                ss << "マウス位置 is X = " << x << " Y = " << y << std::flush;

                ginfo_.font_->render_to( ginfo_.window_->get_renderer(), ss.str() );
            }
        }

    private:
        gui_info ginfo_;

        SDL_Event e;    // ~~~~~
    };

} // namespace ytscript

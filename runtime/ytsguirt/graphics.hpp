#include <memory>
#include <string>
#include <iostream>
#include <cassert>

#include <SDL2/SDL.h>
#include <SDL2/SDL_ttf.h>


namespace ytscript
{
    namespace graphics
    {
        namespace sdl
        {
            template<typename T, typename D>
            class unique_resource
            {
            public:
                unique_resource( T* p, D d )
                    : p_( p )
                    , d_( d )
                {
                    std::cout << "resource init" << std::endl;
                }

                ~unique_resource()
                {
                    if ( p_ != nullptr ) {
                        d_( p_ );
                        std::cout << "resource deinit" << std::endl;
                    }
                }

                unique_resource( unique_resource const& ) = delete;
                unique_resource& operator=( unique_resource const& ) = delete;

                unique_resource( unique_resource&& rhs )
                    : p_( std::move( rhs.p_ ) )
                    , d_( std::move( rhs.d_ ) )
                {
                    rhs.reset();
                }

                unique_resource& operator=( unique_resource&& rhs )
                {
                    if ( &rhs != this ) {
                        p_ = std::move( rhs.p_ );
                        d_ = std::move( rhs.d_ );

                        rhs.reset();
                    }

                    return *this;
                }

            public:
                void reset()
                {
                    p_ = nullptr;
                }

            public:
                inline auto get()
                    -> T*
                {
                    return p_;
                }

                inline auto get() const
                    -> T const*
                {
                    return p_;
                }

                auto operator->()
                    -> T*
                {
                    return get();
                }

                auto operator->() const
                    -> T const*
                {
                    return get();
                }

            private:
                T* p_;
                D d_;
            };

            template<typename T, typename D>
            auto make_resource( T* p, D&& deleter )
            {
                return unique_resource<T, D>( p, std::forward<D>( deleter ) );
            }


            class font
            {
            public:
                font( std::string const& fontname, int const& fontsize )
                    : font_( TTF_OpenFont( fontname.c_str(), fontsize ) )
                {
                    if ( font_ == nullptr ) {
                        assert( false && "failed to open font" );
                    }
                }

                ~font()
                {
                    if ( font_ != nullptr ) {
                        TTF_CloseFont( font_ );
                    }
                }

                font( font const& ) = delete;
                font& operator=( font const& ) = delete;

                font( font&& f )
                {
                    //
                    font_ = f.font_;

                    //
                    f.font_ = nullptr;
                }

            private:
                auto make_surface( std::string const& s )
                {
                    // TODO: fix
                    SDL_Color c = { 0x0, 0x0, 0x0 };

                    auto surface_p = TTF_RenderUTF8_Blended( font_, s.c_str(), c );
                    assert( surface_p != nullptr );
                    return make_resource( surface_p, &::SDL_FreeSurface );
                }

                template<typename Renderer>
                auto make_texture( Renderer& renderer, std::string const& s )
                {
                    auto surface = make_surface( s );
                    auto texture_p
                        = SDL_CreateTextureFromSurface( renderer, surface.get() );
                    assert( texture_p != nullptr );
                    return make_resource( texture_p, &::SDL_DestroyTexture );
                }

            public:
                // TODO: fix
                void render_to( SDL_Renderer* renderer, std::string const& s );

            private:
                TTF_Font* font_;
            };


            class fontsystem
            {
            public:
                fontsystem()
                {
                    if ( TTF_Init() != 0 ) {
                        assert( false && "failed to init font" );
                    }
                }

                ~fontsystem()
                {
                    TTF_Quit();
                }

                fontsystem( fontsystem const& ) = delete;
                fontsystem& operator=( fontsystem const& ) = delete;
                fontsystem( fontsystem&& ) = delete;
                fontsystem& operator=( fontsystem&& ) = delete;

                auto make_font(
                    std::string const& fontname,
                    int const& fontsize
                    ) const
                {
                    return std::make_shared<font>( fontname, fontsize );
                }
            };


            class window
            {
            public:
                window()
                {
                    // TODO: fix
                    window_ = SDL_CreateWindow(
                        "title",
                        SDL_WINDOWPOS_UNDEFINED,        // initial x pos
                        SDL_WINDOWPOS_UNDEFINED,        // initial y pos
                        640,                            // width
                        480,                            // height
                        SDL_WINDOW_OPENGL               // flags
                        );
                    assert( window_ != nullptr );

                    renderer_ = SDL_CreateRenderer( window_, -1, SDL_RENDERER_SOFTWARE );
                    assert( renderer_ != nullptr );
                }

                ~window()
                {
                    if ( renderer_ ) {
                        SDL_DestroyRenderer( renderer_ );
                    }

                    if ( window_ ) {
                        SDL_DestroyWindow( window_ );
                    }
                }

                window( window const& ) = delete;
                window& operator=( window const& ) = delete;
                window( window&& ) = delete;
                window& operator=( window&& ) = delete;

                // TODO: fix these functions
            public:
                auto begin_render()
                {
                    if ( renderer_ ) {
                        ::SDL_SetRenderDrawColor( renderer_, 255, 255, 255, 255 );
                        ::SDL_RenderClear( renderer_ );
                    }
                }

                auto end_render()
                {
                    if ( renderer_ ) {
                        ::SDL_RenderPresent( renderer_ );
                    }
                }

                // TODO: remove
                auto get_renderer()
                    -> SDL_Renderer*
                {
                    return renderer_;
                }

            private:
                SDL_Window* window_ = nullptr;
                SDL_Renderer* renderer_ = nullptr;
            };


            class context
            {
            public:
                context()
                {
                    if ( SDL_Init( SDL_INIT_VIDEO ) != 0 ) {
                        assert( false && "failed: SDL_Init" );
                    }

                    std::cout << "SDL_Init" << std::endl;

                    fontsystem_ = std::make_unique<fontsystem>();
                }

                ~context()
                {
                    SDL_Quit();

                    std::cout << "SDL_Quit" << std::endl;
                }

                context( context const& ) = delete;
                context& operator=( context const& ) = delete;
                context( context&& ) = delete;
                context& operator=( context&& ) = delete;

            public:
                auto make_font( std::string const& fontname, int const& fontsize )
                {
                    return fontsystem_->make_font( fontname, fontsize );
                }

                auto make_window()
                {
                    return std::make_shared<window>();
                }

            private:
                std::unique_ptr<fontsystem> fontsystem_;
            };

        }
    }
}

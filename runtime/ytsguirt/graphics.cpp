#include "graphics.hpp"


namespace ytscript
{
    namespace graphics
    {
        namespace sdl
        {
            // TODO: fix
            void font::render_to( SDL_Renderer* renderer, std::string const& s )
            {
                auto tex = make_texture( renderer, s );
                int w, h;
                ::SDL_QueryTexture( tex.get(), nullptr, nullptr, &w, &h );
                ::SDL_Rect render_quad = { 0, 0, w, h };

                SDL_RenderCopy( renderer, tex.get(), nullptr, &render_quad );
            }

        } // namespace sdl
    } // namespace graphics
} // namespace ytscript

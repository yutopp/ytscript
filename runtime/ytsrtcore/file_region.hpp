#pragma once

#include <array>

#include <boost/interprocess/file_mapping.hpp>
#include <boost/interprocess/mapped_region.hpp>
#include <boost/range/iterator_range.hpp>

#include "defines.hpp"


namespace ytscript
{
    namespace bip = boost::interprocess;

    class file_mapped_region
    {
    public:
        file_mapped_region( char const* const filename )
            : mapping_( filename, bip::read_only )
            , region_( mapping_, bip::read_only )
        {}

    public:
        template<std::size_t N, typename T = byte_t>
        auto read( std::size_t const& offset = 0 ) const
        {
            std::array<T, N> buffer = {{}};

            size_check( N, offset );
            auto p = static_cast<T*>( region_.get_address() );
            std::copy(
                p + offset,
                p + offset + N,
                buffer.data()
                );

            return buffer;
        }

        template<typename T = byte_t const>
        auto make_no_ownership_range(
            std::size_t const& size,
            std::size_t const& offset = 0
            ) const
        {
            size_check( size, offset );
            auto p = static_cast<T*>( region_.get_address() );

            return boost::iterator_range<T*>( p + offset, p + offset + size );
        }

    private:
        void size_check(
            std::size_t const& size,
            std::size_t const& offset
            ) const
        {
            if ( offset + size > region_.get_size() ) {
                assert( false && "overflow" );
            }
        }

    private:
        bip::file_mapping mapping_;
        bip::mapped_region region_;
    };

} // namespace ytscript

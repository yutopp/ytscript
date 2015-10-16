#pragma once

#include <algorithm>
#include <type_traits>
#include <iostream>

#include "file_region.hpp"
#include "section.hpp"


namespace ytscript
{
    class module
    {
    public:
        module( char const* const mod_name )
        {
            auto region = std::make_shared<file_mapped_region>( mod_name );
            parse_to_module( region );
        }

    private:
        template<typename T>
        void parse_to_module( T const& p )
        {
            /*
              header format:
              0: signature:    4Bytes
              4: *reserved*:   4Bytes
              8: version:      4Bytes

              12: offset_to_cs: 4Bytes  // Code Segment
              16: size_of_cs:   4Bytes

              20: offset_to_ds: 4Bytes  // Data Segment
              24: size_of_ds:   4Bytes
            */

            auto const header = p->template read<4>();
            if ( !std::equal( header.begin(), header.end(), "jsp1" ) ) {
                std::cerr << "header is invalid" << std::endl;
                throw 1;
            }

            auto const version_s = p->template read<4>( 8 );
            auto const version =
                *reinterpret_cast<std::int32_t const*>( version_s.data() );
            assert( version == 0x01 );

            // cs
            auto const& offset_to_cs_s = p->template read<4>( 12 );
            auto const offset_to_cs =
                *reinterpret_cast<std::int32_t const*>( offset_to_cs_s.data() );
            auto const& size_of_cs_s = p->template read<4>( 16 );
            auto const size_of_cs =
                *reinterpret_cast<std::int32_t const*>( size_of_cs_s.data() );
            auto cs_range = p->make_no_ownership_range( size_of_cs, offset_to_cs );

            // ds
            auto const& offset_to_ds_s = p->template read<4>( 20 );
            auto const offset_to_ds =
                *reinterpret_cast<std::int32_t const*>( offset_to_ds_s.data() );
            auto const& size_of_ds_s = p->template read<4>( 24 );
            auto const size_of_ds =
                *reinterpret_cast<std::int32_t const*>( size_of_ds_s.data() );
            auto ds_range = p->make_no_ownership_range( size_of_ds, offset_to_ds );
            assert( ds_range[size_of_ds-1] == 0x00 && "not null terminated" );

            std::cout << "code: " << offset_to_cs << " / " << size_of_cs << std::endl;
            std::cout << "data: " << offset_to_ds << " / " << size_of_ds << std::endl;

            code_section_ = code_section( p, cs_range );
            data_section_ = data_section( p, ds_range );
        }

    public:
        code_section code_section_;
        data_section data_section_;
    };

} // namespace ytscript

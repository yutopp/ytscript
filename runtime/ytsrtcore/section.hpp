#pragma once

#include <memory>

#include <boost/range/iterator_range.hpp>

#include "file_region.hpp"


namespace ytscript
{
    class section
    {
    public:
        using owner_region = file_mapped_region;
        using owner_region_ptr = std::shared_ptr<owner_region>;

        using buffer_range = boost::iterator_range<byte_t const*>;

    public:
        section() = default;

        section( owner_region_ptr const& owner, buffer_range const& range )
            : owner_( owner )
            , range_( range )
        {}

    protected:
        owner_region_ptr owner_;
        buffer_range range_;
    };


    class data_section : public section
    {
    public:
        using section::section;

    public:
        auto ref_as_text( std::size_t const& offset ) const
            -> char const*
        {
            assert( offset < this->range_.size() );
            auto p = this->range_.begin() + offset;
            return reinterpret_cast<char const*>( p );
        }
    };


    class code_section : public section
    {
    public:
        using section::section;

    public:
        template<typename T, std::size_t Size>
        auto take()
        {
            static_assert( sizeof(T) <= Size, "" );
            region_check<Size>();

            auto val =
                *reinterpret_cast<T const*>( this->range_.begin() + ip_ );

            ip_ += Size;

            return val;
        }

        template<std::size_t Size>
        void inline step()
        {
            region_check<Size>();

            ip_ += Size;
        }

    public:
        void set_ip( std::size_t const& ip )
        {
            ip_ = ip;
        }

        auto get_ip() const
        {
            return ip_;
        }

    private:
        template<std::size_t Size>
        inline void region_check() const
        {
            if ( ip_ >= this->range_.size() ) {
                assert( false && "ip >= size" );
            }

            if ( ip_ + Size > this->range_.size() ) {
                assert( false && "ip >= size" );
            }
        }

    private:
        std::size_t ip_ = 0;
    };

} // namespace ytscript

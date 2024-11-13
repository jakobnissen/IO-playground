# Unbuffered, readable type
struct Unbuffered
    x::IOBuffer
end

Buffering(::Type{Unbuffered}) = NotBuffered()

function unsafe_read(x::Unbuffered, ptr::Ptr{UInt8}, n::UInt)
    n = min(Base.bytesavailable(x.x) % UInt, n)
    Base.unsafe_read(x.x, ptr, n)
    n % Int
end
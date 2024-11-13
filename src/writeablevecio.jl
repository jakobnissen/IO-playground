# Use a Vector as a writeable IO
struct WriteableVecIO
    vec::Vector{UInt8}
end

# Core methods
flush(::WriteableVecIO) = nothing

function unsafe_write(x::WriteableVecIO, p::Ptr{UInt8}, n::UInt)
    iszero(n) && return 0
    vec = x.vec
    oldlen = length(vec)
    resize!(vec, oldlen + n % Int)
    GC.@preserve vec unsafe_copyto!(pointer(vec, oldlen + 1), p, n)
    n % Int
end
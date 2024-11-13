# Buffered read/writable type
mutable struct VecIO
    const vec::Vector{UInt8}
    pos::Int # in 1:length(vec) + 1

    VecIO() = new(UInt8[], 1)
end

# Core methods
getbuffer(x::VecIO) = @inbounds ImmutableMemoryView(x.vec)[x.pos:lastindex(x.vec)]
fillbuffer(x::VecIO) = 0

function consume(x::VecIO, n::UInt)
    new = x.pos + n
    new > length(x.vec) + 1 && throw_consume_error()
    x.pos = new
end

flush(::VecIO) = nothing

function unsafe_write(x::VecIO, p::Ptr{UInt8}, n::UInt)
    iszero(n) && return 0
    vec = x.vec
    oldlen = length(vec)
    resize!(vec, oldlen + n % Int)
    GC.@preserve vec unsafe_copyto!(pointer(vec, oldlen + 1), p, n)
    n % Int
end

# Extra stuff
function seek(x::VecIO, pos::Int)
    pos ∈ 0:length(x.vec) || error()
    x.pos = pos + 1
    x
end

seekend(x::VecIO) = seek(x, length(x.vec))
position(x) = x.pos - 1


module iotest

using MemoryViews: MemoryView, ImmutableMemoryView, MutableMemoryView
using Test

### Core methods
abstract type Buffering end
struct IsBuffered <: Buffering end
struct NotBuffered <: Buffering end

Buffering(::Type) = IsBuffered()

struct FullBufferError <: Exception end
struct ConsumedTooMuch <: Exception end

@noinline throw_consume_error() = throw(ConsumedTooMuch())

### Derived methods
readinto!(io, mem::MutableMemoryView{UInt8}) = _readinto!(Buffering(typeof(io)), io, mem)

function _readinto!(::IsBuffered, io, mem::MutableMemoryView{UInt8})
    isempty(mem) && return 0
    buf = @something get_nonempty_buffer(io) return 0
    mn = min(length(buf), length(mem))
    copy!(view(mem, 1:mn), view(buf, 1:mn))
    consume(io, mn % UInt)
    return mn
end

function _readinto!(::NotBuffered, io, mem::ImmutableMemoryView{UInt8})
    GC.@preserve mem unsafe_read(io, pointer(mem), length(mem) % UInt)
end

# Default impl of unsafe_read: Convert ref to pointer 
unsafe_read(io, ref, n::UInt) = GC.@preserve ref unsafe_read(io, Ptr{UInt8}(pointer(ref)), n)

# Default impl of unsafe_read, dispatch on buffering to hit fallback when IO is buffered
# This throws a MethodError if IO isn't buffered and user hasn't provided an impl of unsafe_read
unsafe_read(io, ptr::Ptr{UInt8}, n::UInt) = _unsafe_read(Buffering(typeof(io)), io, ptr, n)

# For buffered IOs, we can read from the buffer.
function _unsafe_read(::IsBuffered, io, ptr::Ptr{UInt8}, n::UInt)
    buffer = @something get_nonempty_buffer(io) return 0
    mn = min(n, length(buffer) % UInt)
    GC.@preserve buffer unsafe_copyto!(ptr, pointer(buffer), mn)
    mn % Int
end

function _readinto!(::NotBuffered, io, mem::MutableMemoryView{UInt8})
    isempty(mem) && return 0
    GC.@preserve mem unsafe_read(io, pointer(mem), sizeof(mem) % UInt)
end

function write(io, x)
    mem = ImmutableMemoryView{UInt8}(x)
    isempty(mem) && return 0
    GC.@preserve mem unsafe_write(io, pointer(mem), length(mem) % UInt)
end

# If String is ever backed by memory, this can use the safe memory-based function
function write(io, str::Union{String, SubString{String}})
    isempty(str) && return 0
    GC.@preserve str unsafe_write(io, pointer(str), sizeof(str) % UInt)
end

# If Symbol is ever backed by memory, this can use the safe memory-based function
function write(io, s::Symbol)
    pname = Base.unsafe_convert(Ptr{UInt8}, s)
    return unsafe_write(io, pname, ccall(:strlen, Csize_t, (Cstring,), pname))
end

# helper function
function get_nonempty_buffer(io)::Union{ImmutableMemoryView{UInt8}, Nothing}
    buffer = getbuffer(io)
    if isempty(buffer)
        fillbuffer(io)
        buffer = getbuffer(io)
        isempty(buffer) && return nothing
    end
    buffer
end

bytesavailable(io) = length(getbuffer(io))
eof(io) = isnothing(get_nonempty_buffer(io))
read(io, ::Type{String}) = String(read(io))

# Should this be documented?
seekstart(io) = seek(io, 0)
skip(io, n) = seek(io, position(io) + n)

function readavailable(io)
    buf = getbuffer(io)
    v = Vector{UInt8}(buf)
    consume(io, length(buf) % UInt)
    v
end

# IOStream in particular has an all keyword. We keep this for backwards
# compat, but we DO NOT want the API to contain this keyword in general.
# For this usecase, use readall!
read(io, nb::Int=typemax(Int)) = _read(Buffering(typeof(io)), io, nb)

function _read(::IsBuffered, io, nb::Int)
    v = UInt8[]
    n_read = 0
    while n_read < nb
        buffer = getbuffer(io)
        if isempty(buffer)
            fb = fillbuffer(io)
            iszero(fb) && break
            buffer = getbuffer(io)
        end
        if n_read + length(buffer) > nb
            buffer = @inbounds buffer[1:nb - n_read]
        end
        resize!(v, n_read + length(buffer))
        unsafe_copyto!(@inbounds(MemoryView(v)[n_read+1:n_read+length(buffer)]), buffer)
        consume(io, length(buffer) % UInt)
        n_read += length(buffer)
    end
    v
end

function _read(::NotBuffered, io, nb::Int)
    v = UInt8[]
    bufsize = 2^14 # 16 KiB
    n_read = 0
    while n_read < nb
        oldsize = length(v)
        resize!(v, oldsize + bufsize)
        n = readinto!(io, @inbounds MemoryView(v)[oldsize+1:oldsize+bufsize])
        iszero(n) && break
        n_read += n
    end
    resize!(v, n_read)
end

function readall!(io, mem::MutableMemoryView{UInt8})
    remaining = mem[begin:end]
    n_read = 0
    while true
        n = readinto!(io, remaining)
        iszero(n) && break
        n_read += n
        remaining = mem[begin + n:end]
    end
    return n_read
end

function peek(io, T::Type)
    sz = sizeof(T)
    buffer = getbuffer(io)
    bufz = length(buffer)
    while bufz < sz
        n = fillbuffer(io)
        bufz += n
        if bufz < sz
            iszero(n) && throw(EOFError())
        else
            buffer = getbuffer(io)
        end
    end
    load_from(T, buffer)
end

read(io, T::Type) = read(Buffering(typeof(io)), io, T)

function read(::IsBuffered, io, T::Type)
    y = peek(io, T)
    consume(io, sizeof(T) % UInt)
    y
end

function read(::NotBuffered, io, T::Type)
    sz = sizeof(T)
    mem = Memory{UInt8}(undef, sz)
    n = readinto!(io, MemoryView(mem))
    n < sz && throw(EOFError())
    load_from(T, ImmutableMemoryView(mem))
end

# The caller must guarantee that mem is long enough.
load_from(::Type{Int8}, mem::ImmutableMemoryView{UInt8}) = load_from(UInt8, mem) % Int8
load_from(::Type{T}, mem::ImmutableMemoryView{UInt8}) where T <: Union{Int16, Float16} = load_from(UInt16, mem) % T
load_from(::Type{T}, mem::ImmutableMemoryView{UInt8}) where T <: Union{Int32, Float32} = load_from(UInt32, mem) % T
load_from(::Type{T}, mem::ImmutableMemoryView{UInt8}) where T <: Union{Int64, Float64} = load_from(UInt64, mem) % T
load_from(::Type{Int128}, mem::ImmutableMemoryView{UInt8}) = load_from(UInt128, mem) % Int128

function load_from(::Type{UInt8}, mem::ImmutableMemoryView{UInt8})
    @inbounds mem[1]
end

function load_from(::Type{UInt16}, mem::ImmutableMemoryView{UInt8})
    ltoh(@inbounds(mem[1] % UInt16 | ((mem[2] % UInt16) << 8)))
end

function load_from(::Type{T}, mem::ImmutableMemoryView{UInt8}) where {T <: Union{UInt32, UInt64, UInt128}}
    x = zero(T)
    msk = 8 * sizeof(T) - 1
    for i in 1:sizeof(T)
        x |= (@inbounds(mem[i]) % T) << ((8(i-1)) & msk)
    end
    ltoh(x)
end

# Users only need to implement writing for the unsigned integer types
write(io, x::Int8) = write(io, reinterpret(UInt8, x))
write(io, x::Union{Int16, Float16}) = write(io, reinterpret(UInt16, x))
write(io, x::Union{Int32, Float32}) = write(io, reinterpret(UInt32, x))
write(io, x::Union{Int64, Float64}) = write(io, reinterpret(UInt64, x))
write(io, x::Int128) = write(io, reinterpret(UInt128, x))

# TODO: These defaults are very inefficient. Should we even have them?
function write(io, x::UInt8)
    mem = Memory{UInt8}(undef, 1)
    mem[1] = x
    write(io, mem)
end

function write(io, x::UInt16)
    x = htol(x) # make sure it's little endian
    mem = Memory{UInt8}(undef, 2)
    mem[1] = x % UInt8
    mem[2] = (x >> 8) % UInt8
    write(io, mem)
end

function write(io, x::Union{UInt32, UInt64, UInt128})
    x = htol(x)
    mem = Memory{UInt8}(undef, sizeof(x))
    msk = 8 * sizeof(x) - 1
    for i in 0:sizeof(x)-1
        mem[i] = x >> (8i & msk) % UInt8
    end
    write(io, mem)
end















#=


# TODO: These implementations are not great. Should be revisited
function readbytes!(io, b::AbstractVector{UInt8}, nb::Int=length(b); all::Bool=true)
    iszero(nb) && return 0
    _readbytes(MemoryKind(typeof(b)), Buffering(typeof(io)), io, b, nb, all)
end

function _readbytes(
    ::IsMemory{MutableMemoryView{UInt8}},
    ::IsBuffered,
    io, b, nb, all
)
    if all
        n_read = 0
        while n_read < nb
            buffer = @something get_nonempty_buffer(io) return n_read
            @assert length(buffer) > 0
            to_read = min(length(buffer), nb - n_read)
            if length(b) < n_read + to_read
                resize!(b, n_read + to_read)
            end
            dst = (MemoryView(b)[n_read+1:n_read + to_read])::MutableMemoryView{UInt8}
            @assert length(dst) == to_read
            src = buffer[1:to_read]
            copyto!(dst, src)
            consume(io, to_read)
            n_read += to_read
        end
        @assert n_read == nb
        return nb
    else
        buffer = @something get_nonempty_buffer(io) return 0
        to_read = min(length(buffer), nb)
        if length(b) < to_read
            resize!(b, to_read)
        end
        dst = MemoryView(b)::MutableMemoryView{UInt8}
        src = buffer[1:to_read]
        copyto!(dst, src)
        consume(io, to_read)
        return to_read
    end
end

function _readbytes(
    ::IsMemory{MutableMemoryView{UInt8}},
    ::NotBuffered,
    io, b, nb, all
)
    chunksize = 2^12 # 4 KiB
    if all
        n_read = 0
        while n_read < nb

        end
    else
        isempty(b) && resize!(b, chunksize)
        readbytes!(io, MemoryView(b))
    end
end

=#

include("memio.jl")
include("devnull.jl")
include("buffered.jl")
include("unbuffered.jl")
include("vecio.jl")

end # module iotest



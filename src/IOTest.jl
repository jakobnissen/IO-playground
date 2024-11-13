module IOTest

using MemoryViews: MemoryView, ImmutableMemoryView, MutableMemoryView
using Test

### Core methods

"""
    Buffering(::Type{T}) -> Union{IsBuffered, NotBuffered}

Trait to signal if a reader IO is buffered.
Defaults to `IsBuffered()`.
New unbuffered io types `T` should signal they are unbuffered by implementing
`Buffering(::Type{T}) = NotBuffered()`.
"""
abstract type Buffering end

"See `Buffering`"
struct IsBuffered <: Buffering end

"See `Buffering`"
struct NotBuffered <: Buffering end

Buffering(::Type) = IsBuffered()

"Exception thrown when calling `fillbuffer` on an IO with a full buffer that cannot grow"
struct FullBufferError <: Exception end

"Exception thrown when calling consume(io, n) with a too large value of n"
struct ConsumedTooMuch <: Exception end

@noinline throw_consume_error() = throw(ConsumedTooMuch())

### Derived methods

# Default impl of unsafe_read: Convert ref to pointer
unsafe_read(io, ref, n::UInt) = GC.@preserve ref unsafe_read(io, Ptr{UInt8}(pointer(ref)), n)

# Dispatch on buffering: A buffered reader need not implement unsafe_read,
# but will get it for free by copying bytes from its buffer
unsafe_read(io, ptr::Ptr{UInt8}, n::UInt) = _unsafe_read(Buffering(typeof(io)), io, ptr, n)

# For buffered IOs, we can read from the buffer.
function _unsafe_read(::IsBuffered, io, ptr::Ptr{UInt8}, n::UInt)
    buffer = @something get_nonempty_buffer(io) return 0
    mn = min(n, length(buffer) % UInt)
    GC.@preserve buffer unsafe_copyto!(ptr, pointer(buffer), mn)
    mn % Int
end

"""
    readinto!(io, x)

Read bytes from `io` into `x` (interpreted as a `MemoryView`),
and return the number of bytes read.
This function should only read zero bytes if `x` contain no bytes, or if `io` is EOF.
"""
readinto!(io, x) = readinto!(io, MemoryView{UInt8}(x))

function readinto!(io, mem::MutableMemoryView{UInt8})
    isempty(mem) && return 0
    GC.@preserve mem unsafe_read(io, pointer(mem), sizeof(mem) % UInt)
end

# Generic implementation of write for any memory-based object.
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

# internal helper function - not API
# returns a nonempty buffer, unless IO is EOF
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

# By default, only buffered readers have a notion
# of the number of bytes available.
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

# For buffered IOs, this is pretty straightforward
function _read(::IsBuffered, io, nb::Int)
    v = UInt8[]
    n_read = 0
    while n_read < nb
        buffer = @something get_nonempty_buffer(io) break
        if n_read + length(buffer) > nb
            # Don't read too much
            buffer = @inbounds buffer[1:nb - n_read]
        end
        resize!(v, n_read + length(buffer))
        unsafe_copyto!(@inbounds(MemoryView(v)[n_read+1:n_read+length(buffer)]), buffer)
        consume(io, length(buffer) % UInt)
        n_read += length(buffer)
    end
    v
end

# This is a little more tricky, because for unbuffered IOs, we might not
# have `bytesavailable`. So, we need to overallocate some extra space,
# then try to fill it in, and resize back if we reach EOF early.
function _read(::NotBuffered, io, nb::Int)
    v = UInt8[]
    bufsize = 2^12 # 4 KiB
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

readall!(io, x) = readall!(io, MemoryView{UInt8}(x))
function readall!(io, mem::MutableMemoryView{UInt8})
    n_read = 0
    while true
        n = readinto!(io, mem)
        iszero(n) && break
        n_read += n
        mem = @inbounds mem[begin + n:end]
    end
    return n_read
end

# We load it from the buffer
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
    # This read method is new - not in Base
    unsafe_read(T, @inbounds(buffer[1:sz]))
end

read(io, T::Type) = read(Buffering(typeof(io)), io, T)

# For buffered ones, we can load directly from buffer
function read(::IsBuffered, io, T::Type)
    y = peek(io, T)
    consume(io, sizeof(T) % UInt)
    y
end

# Else, we need to read into a Memory, then load from it
function read(::NotBuffered, io, T::Type)
    sz = sizeof(T)
    mem = Memory{UInt8}(undef, sz)
    n = readinto!(io, MemoryView(mem))
    n < sz && throw(EOFError())
    unsafe_read(T, ImmutableMemoryView(mem))
end

# New method.
# The caller must guarantee that mem is long enough.
# By default, we just load them as unsigned ints, then reinterpret
unsafe_read(::Type{Int8}, mem::ImmutableMemoryView{UInt8}) = unsafe_read(UInt8, mem) % Int8
unsafe_read(::Type{T}, mem::ImmutableMemoryView{UInt8}) where T <: Union{Int16, Float16} = unsafe_read(UInt16, mem) % T
unsafe_read(::Type{T}, mem::ImmutableMemoryView{UInt8}) where T <: Union{Int32, Float32} = unsafe_read(UInt32, mem) % T
unsafe_read(::Type{T}, mem::ImmutableMemoryView{UInt8}) where T <: Union{Int64, Float64} = unsafe_read(UInt64, mem) % T
unsafe_read(::Type{Int128}, mem::ImmutableMemoryView{UInt8}) = unsafe_read(UInt128, mem) % Int128

function unsafe_read(::Type{UInt8}, mem::ImmutableMemoryView{UInt8})
    @inbounds mem[1]
end

function unsafe_read(::Type{UInt16}, mem::ImmutableMemoryView{UInt8})
    ltoh(@inbounds(mem[1] % UInt16 | ((mem[2] % UInt16) << 8)))
end

function unsafe_read(::Type{T}, mem::ImmutableMemoryView{UInt8}) where {T <: Union{UInt32, UInt64, UInt128}}
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

# TODO: These defaults are very inefficient because they need to
# allocate a Memory. Should we even have them?
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

readchomp(io) = chomp(read(io, String))

# Forward this to Base since I don't want to implement stdout yet
readline(;keep::Bool=false) = Base.readline(;keep)

# Not do I yet care to implement opening a file and reading it.
readline(filename::AbstractString; keep::Bool=false) = Base.readline(filename; keep)

include("writeablevecio.jl")

# We use the WriteableVecIO here to not have to allocate a heavyweight IOBuffer
readline(io; keep::Bool=false) = String(copyline(WriteableVecIO(UInt8[]), io; keep))

# We forward as we don't want to implement IOStream yet
copyline(io, filename::AbstractString; keep::Bool=false) = Base.copyline(io, filename; keep)

# TODO: We could implement this in terms of copyuntil
# We can assume in_io is buffered, because copyline does not make sense
# for unbuffered IOs
function copyline(out_io, in_io; keep::Bool=false)
    buffer = get_nonempty_buffer(in_io)
    isnothing(buffer) && return out_io
    while true
        newline = findfirst(==(UInt8('\n')), buffer)
        if newline === nothing
            write(out_io, buffer)
            consume(in_io, length(buffer) % UInt)
            iszero(fillbuffer(in_io)) && break
            buffer = getbuffer(in_io)
        else
            if keep
                buffer = @inbounds buffer[1:newline]
            else
                win_line = newline > 1 && @inbounds(buffer[newline - 1]) == UInt8('\r')
                buffer = @inbounds buffer[1:newline - (1 + win_line)]
            end
            write(out_io, buffer)
            consume(in_io, newline % UInt)
            break
        end
    end
    out_io
end

readlines(io=stdin; kw...) = collect(eachline(io; kw...))

# TODO: readuntil, copyuntil, readeach, eachline
# TODO: readbytes!

include("memio.jl")
include("devnull.jl")
include("buffered.jl")
include("unbuffered.jl")
include("vecio.jl")


end # module iotest



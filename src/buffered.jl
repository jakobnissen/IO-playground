# Buffered, readable type that converts any readable type into a buffered one
mutable struct BufferedIO{T}
    const x::T
    const buffer::Memory{UInt8}
    fst::UInt # First byte with readable data. In 1:lst+1
    lst::UInt # Last byte with readable data. In 0:length(buffer).

    function BufferedIO(x, bufsize::Int=2^14)
        bufsize < 1 && error("Buffer must contain at least one byte")
        buffer = Memory{UInt8}(undef, bufsize)
        new{typeof(x)}(x, buffer, 1, 0)
    end
end

getbuffer(x::BufferedIO) = @inbounds ImmutableMemoryView(x.buffer)[x.fst % Int:x.lst % Int]

function fillbuffer(x::BufferedIO)
    (fst, buffer) = (x.fst, x.buffer)
    n_bytes = x.lst - fst + 1
    # Copy any remaining data to beginning of buffer
    if fst > 1 && !iszero(n_bytes)
        unsafe_copyto!(buffer, 1, buffer, fst, n_bytes)
    elseif n_bytes == length(buffer)
        throw(FullBufferError())
    end
    n = readinto!(x.x, @inbounds(MemoryView(buffer)[n_bytes % Int + 1:end]))
    x.fst = 1
    x.lst = n_bytes + n
    return n
end

function consume(x::BufferedIO, i::UInt)
    new = x.fst + i
    new > x.lst + 1 && throw_consume_error()
    x.fst = new
    nothing
end
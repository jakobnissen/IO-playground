# Buffered, readable type
mutable struct MemIO
    mem::ImmutableMemoryView{UInt8}

    MemIO(x::ImmutableMemoryView{UInt8}) = new(x)
    MemIO(x) = MemIO(ImmutableMemoryView{UInt8}(x))
end

getbuffer(mem::MemIO) = mem.mem
fillbuffer(mem::MemIO) = 0

function consume(mem::MemIO, i::UInt)
    buffer = getbuffer(mem)
    @boundscheck i > length(buffer) && throw_consume_error()
    mem.mem = @inbounds buffer[i % Int + 1:end]
end
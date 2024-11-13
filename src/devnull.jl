# Buffered, read/writeable type
struct DevNull end

const DEV_NULL_MEM = ImmutableMemoryView(Memory{UInt8}())

getbuffer(::DevNull) = DEV_NULL_MEM
fillbuffer(::DevNull) = 0
consume(::DevNull, i::UInt) = iszero(i) ? nothing : throw_consume_error()
unsafe_write(::DevNull, ptr::Ptr{UInt8}, n::UInt) = Int(n)
flush(::DevNull) = nothing
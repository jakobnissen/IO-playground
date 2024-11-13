# I THINK these are correct - not sure!

# Core interface - Base already implements unsafe_read and unsafe_write
function getbuffer(io::IOBuffer)
    io.reinit && return DEV_NULL_MEM
    ImmutableMemoryView{UInt8}(io.data)[io.ptr:io.size]
end

fillbuffer(::IOBuffer) = 0

function consume(io::IOBuffer, n::UInt)
    if io.reinit
        iszero(n) && throw_consume_error()
        return nothing
    else
        new = io.ptr + n % Int
        new > io.size + 1 && throw_consume_error()
        io.ptr = new
    end
end

# Optional methods: Not yet implemented!
module SizedArrays
    export NVector
    
    # dynamically allocated vector, but with buffer size known at compile time
    struct NVector{N, T}
        data::Vector{T}
        NVector{N, T}() where {N, T} = new(Vector{T}(undef, N))
    end
end
def states(m: BigInt, c: BigInt = 1): LazyList[(BigInt, BigInt)] = 
    if (m < c) LazyList.empty else 
    (m, c) #:: states((m - c) * 2, c * 2)



states(100).force
val zu: Seq[Int] = Array(1, 2, 3)
val z +: u = zu 
def states(m: BigInt, c: BigInt = 1): LazyList[(BigInt, BigInt)] = 
    if (m < c) LazyList.empty else 
    (m, c) #:: states((m - c) * 2, c * 2)



// states(100).force
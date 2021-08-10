package playground.free


def sum(from: Long, to: Long, step: Long = 1): Nark.Eval[Long] = 
    if to < from then Nark.pure(0) else 
        Nark.delay(sum(from + step, to, step).map[Long, Long, Nothing](_ + from))


@main def check() = 
    println(sum(1, 1000000).exec.pivot.value)

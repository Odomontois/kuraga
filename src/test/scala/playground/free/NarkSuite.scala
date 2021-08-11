package playground.free

def sum(from: Long, to: Long, step: Long = 1): Nark.Eval[Long] =
  if to < from then Nark.pure(0)
  else
    for s <- sum(from + step, to, step).delayed
    yield s + from

@main def check() =
  println(sum(2, 300).exec.pivot.value)

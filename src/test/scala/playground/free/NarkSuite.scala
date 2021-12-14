package playground.free

def sum(from: Long, to: Long, step: Long = 1): Nark.Eval[Long] =
  if to < from then Nark.pure(0)
  else
    for s <- sum(from + step, to, step).delayed
    yield s + from

def sum2(from: Long): Nark.Reader[(Long, Long), Long] =
  Nark
    .ask[(Long, Long)]
    .flatMap((to, step) =>
      if to < from then Nark.pure(0)
      else
        for s <- sum2(from + step).delayed
        yield s + from
    )

@main def check() =
  println(sum(2, 300).exec.pivot.value)
  println(sum2(2).provide((300, 1)).exec.pivot.value)

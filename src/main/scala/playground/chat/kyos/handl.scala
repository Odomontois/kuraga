package playground.chat.kyos

import kyo.Choice

object handl {
// val f = summon[Frame]
    val u =
        for
            x <- Choice.eval(1, 2)
            y  = 2 * x
            x <- Choice.eval(3, 4)
            z  = x * 3
        yield y + z

    // Choice.run(u).eval.toList
}

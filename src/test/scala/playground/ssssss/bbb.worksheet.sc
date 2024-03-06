import scala.language.dynamics
import playground.ssssss.*

val a, b, c = Var()

a ~ 2 | 4 and b ~ 3 | 5 and c ~ 6 | 7

for
    x <- 1 to 5
    y <- 1 until 6
yield x + y

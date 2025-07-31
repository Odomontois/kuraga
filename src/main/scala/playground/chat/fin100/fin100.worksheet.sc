import spire.math.Rational

def best(next: Double): Double = {
    val th = next.floor
    val p = th / 100
    p * next + (1 - p) * (th + 1 + 100) / 2
}

def solve(n: Int, acc: Double = 0): Double = if n == 0 then acc else solve(n - 1, best(acc))


solve(10).toDouble
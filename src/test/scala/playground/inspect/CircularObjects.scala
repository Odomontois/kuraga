package playground.inspect



object One {
    val x: String = s"one ${Two.x}"
}

object Two {
    val x: String = s"two ${One.x}"
}

@main def run() = 
    println(s"One: ${One.x}, Two: ${Two.x}")

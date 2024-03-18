package amst
import scala.collection.mutable
import java.time.LocalDate
import language.experimental.captureChecking
import scala.annotation.capability

opaque type Task = String

@capability class MyState private (private var count: Int)

object MyState {
    def count(using state: MyState): Int        = state.count
    def increment()(using state: MyState): Unit = state.count += 1
    def withState[T](actions: MyState^{} ?-> T): T = {
        given MyState(0)
        actions
    }
}

def reportState(using MyState): String => String = {
    val count = MyState.count
    name => s"$name current value is $count\n"
}

def reportState1(using MyState): String => String = {
    name => s"$name current value is ${MyState.count}\n"
}

def compare(using MyState): String = {
    val oldReport = reportState
    for (i <- 0 to 9) {
        MyState.increment()
    }
    val newReport = reportState
    oldReport("old") ++ { newReport("new") }
}

@main def runMyState(): Unit = MyState.withState {
    println(compare)
}

// old current value is 0
// new current value is 10
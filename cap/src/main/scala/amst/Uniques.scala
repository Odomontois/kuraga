package amst.cc
import scala.collection.mutable
import java.time.LocalDate
import language.experimental.captureChecking
import scala.annotation.capability
import scala.caps.cap

opaque type Task = String

@capability class MyState private (private var count: Int)

trait Metric{
    def display(name: String): String
}

class StateMetric(using state: MyState){
  def display(name: String): String = s"$name current value is ${MyState.count}\n"
}

object MyState {
    def count(using state: MyState): Int        = state.count
    def increment()(using state: MyState): Unit = state.count += 1
    def withState[T](actions: MyState^{} ?-> T): T = {
        given MyState(0)
        actions
    }
}

def reportState(using MyState): String -> String = {
    val count = MyState.count
    name => s"$name current value is $count\n"
}

def reportState1(using state:MyState): String ->{state} String = {
    name => s"$name current value is ${MyState.count}\n"
}

def compare(using MyState): () -> String = {
    val oldReport = reportState
    for (i <- 0 to 9) {
        MyState.increment()
    }
    val newReport = reportState
    () => (oldReport("old") ++ { newReport("new") })
}

@main def runMyState(): Unit = println(MyState.withState(compare)())


// old current value is 0
// new current value is 10

class IO

class File{
    def readLine(): String = ???
}

object IO {
    def readLine(using io: IO): String = ???
    def writeLine(using io: IO)(line: String): Unit = ???
    def openFile[A](using io: IO)(use: File ?-> A): A = ???
}

def foo(f: File): String -> String = name => s"Hello, $name ${f.readLine()}!"




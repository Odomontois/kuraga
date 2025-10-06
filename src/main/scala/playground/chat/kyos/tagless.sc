
import kyo.*
import kyo.kernel.Effect
import kyo.kernel.ContextEffect
import kyo.kernel.ArrowEffect

// 1. Define the effect signatures
sealed trait Log extends ArrowEffect[Const[Log.Line], Const[Unit]]

object Log:
    case class Line(level: String, message: String)

    def log(level: String)(message: String): Unit < Log =
        ArrowEffect.suspend[Unit](Tag[Log], Line(level, message))

sealed trait Console extends ArrowEffect[Console.Op, Id]

object Console:
    enum Op[+A]:
        case Readline()              extends Op[String]
        case Printline(line: String) extends Op[Unit]

    def printLine(line: String): Unit < Console =
        ArrowEffect.suspend(Tag[Console], Op.Printline(line))

    def read: String < Console = ArrowEffect.suspend(Tag[Console], Op.Readline())

// 2. Define the program. Notice the clean, self-contained signature.
val program: String < (Log & Console) =
    for
        _    <- Log.log("Debug")("Start teletype example")
        _    <- Console.printLine("Hello, World!")
        _    <- Console.printLine("What is your name?")
        name <- Console.read
        _    <- Console.printLine(s"Hello $name!")
    yield name

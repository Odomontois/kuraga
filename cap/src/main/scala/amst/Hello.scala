package amst

import language.experimental.captureChecking
import scala.annotation.capability
import scala.caps.cap
import scala.caps.Capability
import scala.caps.SharedCapability

@main def hello =
  println("Hello, Oleg2!")

class Console(console: java.io.Console) extends SharedCapability:
  def readLine()(using IO^): String = console.readLine()

class IO extends SharedCapability

object Console:
  def readLine()(using console: Console^)(using IO^): String = console.readLine()

class File(using Files^):
  def writeLine(line: String): Unit = ()

@capability class Files

object Files:
  def openFile(name: String)(using files: Files^)(using IO): File^{files} = File(using files)

// def foo: String -> (files: Files^, io: IO) ?->{} Int -> (Console, IO) ?->{files} Unit =
//     name =>
//         val file = Files.openFile(name)
//         def go(n: Int)(using Console, IO): Unit =
//             if n > 0 then
//                 val line = Console.readLine()
//                 file.writeLine(line)
//                 go(n - 1)
//         i => go(i)

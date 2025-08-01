package amst.uniques

import scala.annotation.capability
import language.experimental.saferExceptions
import java.nio.file.Path
import java.nio.file.AccessMode
import java.io.IOException
import caps.cap

final class Aborted extends Exception("", null, false, false)

type Abort = CanThrow[Aborted]

object Abort:
    def abort(using Abort): Nothing                                 = throw Aborted()
    def getOrElse[T](default: T)(block: DummyImplicit ?=> T throws Aborted): T =
        try block
        catch case _: Aborted => default


@capability trait Get[+A](val value: A)
object Get:
    def get[A](using get: Get[A]^): A = get.value

@capability class State[A](private var state: A):
    def get: A = state
    def set(value: A): Unit = state = value
    def modify(f: A => A): Unit = state = f(state)

object State:
    def of[A](using state: State[A]^): State[A]^{state} = state
    

trait File


def lol1(using Abort, Get[String]^, State[Int]^): String = 
    if State.of.get > 10 then Abort.abort
    State.of.modify(_ + 1)
    s"Name is ${Get.get}"

val lol: (Abort, Get[String]^, State[Int]^) ?-> String = 
    if State.of.get > 10 then Abort.abort
    State.of.modify(_ + 1)
    s"Name is ${Get.get}"

@main def run() = 
    given Get[String]("Scala")
    given State[Int](0)
    println(Abort.getOrElse("Kek")(lol))

class Handle
@capability trait IO
import AccessMode.READ

type IOExceptions = CanThrow[IOException]
trait Example:
    def open(path: Path, mode: AccessMode)(using IO^, IOExceptions): Handle
    def parseInt(string: String)(using Abort): Int
    def getLine(handle: Handle)(using IO^, IOExceptions): String

    def parseFileLines(path: Path)(using IO, IOExceptions):Int -> (IO, IOExceptions, Abort) ?-> Int = 
        val file = open(path, READ)
        i => i + parseInt(getLine(file))        


trait Stream[A]:
    def emit(a: A): Unit


object Stream:
    def emit[A](a: A)(using stream: Stream[A]^): Unit = stream.emit(a)
    @capability abstract class Impl[A] extends Stream[A]:
        def emit(a: A): Unit 

    def map[A, B](f: A -> B)(e: Stream[A]^ ?-> Unit)(using bs: Stream[B]^): Unit = 
        e(using new Stream[A]:
            def emit(a: A) = bs.emit(f(a)))

    def flatMap[A, B](e: Stream[A]^ ?-> Unit)(f: A -> Stream[B]^ ?-> Unit)(using bs: Stream[B]^): Unit = 
        e(using new Stream[A]:
            def emit(a: A) = f(a)(using new Stream[B]:
                def emit(b: B) = bs.emit(b)))

    def filter[A](e: Stream[A]^ ?-> Unit)(p: A -> Boolean)(using bs: Stream[A]^): Unit = 
        e(using new Stream[A]:
            def emit(a: A) = if p(a) then bs.emit(a))

    def take[A](n: Int)(e: Stream[A]^ ?-> Unit)(using bs: Stream[A]^): Unit = 
        var count = 0
        Abort.getOrElse(())(e(using new Stream[A]:
            def emit(a: A) = if count == n then Abort.abort else
                bs.emit(a)
                count += 1
        ))

    def range(from: Int, until: Int)(using stream: Stream[Int]^): Unit = 
        if from < until then 
            stream.emit(from)
            range(from + 1, until)

    def toList1[A](stream: Stream[A]^ ?-> Unit): List[A] = 
        val builder = List.newBuilder[A]
        stream(using new Stream[A]:
            def emit(a: A) = builder += a)
        builder.result()

    def toList[A](stream: Stream[A]^ ?-> Unit): List[A] = 
        val builder = List.newBuilder[A]
        given Stream[A] with
            def emit(a: A) = builder += a
        stream
        builder.result()

    def toList2[A](stream: Stream[A]^ ?-> Unit): List[A] = 
        val builder = List.newBuilder[A]
        stream(using new Stream[A]:
            def emit(a: A) = builder += a)
        builder.result()


@main def runList = 
    println(Stream.toList{
        Stream.range(1, 6)
        Stream.range(10, 16)
        Stream.range(20, 24)
    })

    println(Stream.toList{
        Stream.filter{
            for(i <- 1 to 100) 
                Stream.emit(i * i)
            }(_ < 1000)
    })
    
        


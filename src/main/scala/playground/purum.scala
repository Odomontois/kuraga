package purum

import scala.util.Random.nextPrintableChar
import scala.collection.immutable.WrappedString

@main def pass() =
    println:
        WrappedString.fill(17)(nextPrintableChar)

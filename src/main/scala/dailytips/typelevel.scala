package dailytips.typelevelineq

import scala.compiletime.ops.int.*

def checkNum[
    T <: Int & Singleton,
    positive >: (T >= 0) <: true,
    smallerThenTen >: (T < 10) <: true
](x: T): Unit = ()

@main def foo() =
  checkNum(5)  // ok
//  checkNum(-1) // does not compile
//  checkNum(10) // does not compile
end foo

//> using scala 3.8.0-RC1-bin-20250822-658c8bd-NIGHTLY
//> using options "-language:experimental.captureChecking"
import scala.caps.Capability

open class Marco extends Capability {
  def action(x: Int): Unit = ()
}

val marco: Marco^                   = Marco()
def foo(): (Int => Int, Int => Int) =
  val f1 = (x: Int) =>
    marco.action(x)
    x + 1

  val f2 = (x: Int) =>
    marco.action(x)
    x - 1

  (f1, f2)
end foo

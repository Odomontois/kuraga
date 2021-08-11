package playground.ptest.pack1
import scala.reflect.ClassTag

trait TC[A]:
  def name: String

object TC:
  def derived[A: ClassTag]: TC[A] = new:
    def name = summon[ClassTag[A]].runtimeClass.getName

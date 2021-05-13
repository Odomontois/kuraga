package playground.ptest.pack1
import scala.reflect.ClassTag

trait TC[A]{
  def name: String
}

object TC:
  given derived[A: ClassTag]: TC[A] with
    def name = summon[ClassTag[A]].runtimeClass.getName

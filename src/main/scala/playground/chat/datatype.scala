package playground.chat.datasettype

case class DataX(v: String)

case class DataY(v: Int)

case class DataZ(v: Double)

sealed trait DataType {
  type Data
  def asVal: DataType.Val[Data]
}

object DataType {
  abstract class Val[T] extends DataType {
    type Data = T
    def asVal = this
  }
  case object X extends Val[DataX]
  case object Y extends Val[DataY]
  case object Z extends Val[DataZ]
}

def prepareDataImpl[A](ls: List[String], typ: DataType.Val[A]): List[A] = ???

def prepareData(ls: List[String], typ: DataType): List[typ.Data] = typ match {
  case DataType.X => ls.map(DataX(_))
  case DataType.Y => ls.view.flatMap(_.toIntOption).map(DataY(_)).toList
  case DataType.Z => ls.view.flatMap(_.toDoubleOption).map(DataZ(_)).toList
}

val input = List("1", "1.0", "a")

@main def run() = 
  prepareData(input, DataType.X) : List[DataX]
  prepareData(input, DataType.Y) : List[DataY]
  prepareData(input, DataType.Z) : List[DataZ]
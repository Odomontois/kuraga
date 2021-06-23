package playground.chat.datasettype

case class DataX(v: String)

case class DataY(v: Int)

case class DataZ(v: Double)

sealed trait DataType[Data]

object DataType {
  case object X extends DataType[DataX]
  case object Y extends DataType[DataY]
  case object Z extends DataType[DataZ]
}

def prepareDataImpl[A](ls: List[String], typ: DataType[A]): List[A] = ???

def prepareData[A](ls: List[String], typ: DataType[A]): List[A] = typ match 
  case DataType.X => ls.map(DataX(_))
  case DataType.Y => ls.view.flatMap(_.toIntOption).map(DataY(_)).toList
  case DataType.Z => ls.view.flatMap(_.toDoubleOption).map(DataZ(_)).toList


val input = List("1", "1.0", "a")

@main def run() = 
  prepareData(input, DataType.X) : List[DataX]
  prepareData(input, DataType.Y) : List[DataY]
  prepareData(input, DataType.Z) : List[DataZ]
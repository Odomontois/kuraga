package kuraga.typeclasses


trait Predicate[Name, T]{
  def check(t: T): Boolean
}

delegate for Predicate["Even", Int] = _ % 2 == 0
delegate for Predicate["Small", String] = _.length < 5

case class Storage[T](items: List[T])

delegate for Storage[Int] = Storage(List(2, 4, 5, 1))
delegate for Storage[String] = Storage(List("lol", "kek", "cheburek"))

def filterStorage[Name, T] given (pred: Predicate[Name, T], storage: Storage[T]): List[T] = 
  storage.items.filter(pred.check)


object FilterStorage extends App{
  println(filterStorage[Name = "Even"])
  println(filterStorage[Name = "Small"])
}

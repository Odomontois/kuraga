package playground.typeclasses


trait Predicate[Name, T]{
  def check(t: T): Boolean
}


object Predicate{
  type Even
  type Small

  given Predicate[Even, Int] = _ % 2 == 0
  given Predicate[Small, String] = _.length < 5
  given Predicate[Small, Int] = _ < 5
}

case class Storage[T](items: List[T])

given Storage[Int] = Storage(List(2, 4, 5, 1))
given Storage[String] = Storage(List("lol", "kek", "cheburek"))

def filterStorage[Name, T] (using pred: Predicate[Name, T], storage: Storage[T]): List[T] = 
  storage.items.filter(pred.check)



// object FilterStorage extends App{
//   println(filterStorage[Name = Predicate.Even])
//   println(filterStorage[ Predicate.Small, Int])
// }

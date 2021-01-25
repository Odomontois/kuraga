package playground.data

opaque type NonEmptyList[+A] = List[A]

object NonEmptyList  :
  def apply[A](a: A, as: A*) : NonEmptyList[A] =  a :: List(as :_*)  

  extension [A](self: NonEmptyList[A]) 
    def toList: List[A] = self
    def head: A = self.head
    def tail: List[A] = self.tail

  extension [A, B](self: NonEmptyList[A]) def  map (f: A => B): NonEmptyList[B] = self.map(f)

object NonEmptyListMain:
  def main(args: Array[String]) = 
    val a = NonEmptyList(5, 6, 7, 8)
    println(a)
    println(a.map(_ + 1))
  
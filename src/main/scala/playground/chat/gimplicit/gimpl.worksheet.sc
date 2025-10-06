enum A:
    case WhoYee, Picky

object A:
    given lol: A                                 = A.WhoYee
    given kek(using A): A.Picky.type = A.Picky

enum B:
    case WhoYee, Picky

object B:
    implicit def lol: B                                 = B.WhoYee
    implicit def kek(using B): B.Picky.type = B.Picky

implicit object U

summon[A]
summon[B]


trait Y: 
    def name: String
trait Z

type PP = Y
given (using Option[Int]): Y with {def name = "int"}

given (using Option[String]): PP with {def name = "string"}

{
    given Option[Int] = None
    summon[Y].name
}

{
    given Option[String] = None
    summon[Y].name
}
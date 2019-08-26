package kuraga.inlines


inline def foo(ls: List[String]) <: Any = {
    ls match {
        case List("number", "1") => 1
        case List("number", "2") => 2
        case Nil => "hz"
    }
}

object stringlists extends App{
    val num1 = foo(List("number", "1"))

    val num2 = foo(List("number", "2"))

    val num3 = foo(List("uuu"))

    println(implicitly[ValueOf[num1.type]].value)
    println(implicitly[ValueOf[num2.type]].value)
    println(implicitly[ValueOf[num3.type]].value)
}

type Not[b <: Boolean] <: Boolean = b match {
    case false => true
    case true => false
}

sealed trait Lst[+A]{
    type IsEmpty <: Boolean    
    def isEmpty: IsEmpty
    def notEmpty : Not[IsEmpty] 
}

object Lst{
    case object Nil extends Lst[Nothing]{
        type IsEmpty = true
        def isEmpty = true
        def notEmpty = false
    }
    case class ::[+A](head: A, tail: Lst[A]) extends Lst[A]{
        type IsEmpty = false
        def isEmpty = false
        def notEmpty = true
    }
}

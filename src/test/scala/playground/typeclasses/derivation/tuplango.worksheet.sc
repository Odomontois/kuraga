import scala.compiletime.erasedValue


erasedValue[(Int, String)].map([A] => (x: A) => List(x))
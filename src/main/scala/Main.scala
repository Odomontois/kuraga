object Main {

  extension (s: String) def double: String = s + s

  def main(args: Array[String]): Unit = {
    println("Hello world!")
    println(msg.double)
    // println(double(msg))
  }

  def msg = "I was compiled by dotty :)"

  // trait A:
  // trait B extends A:

  // summon[((x : A) => Int) <:< ((x: B) => Int )]

  def check[A, B <: A, C] = summon[(A ?=> C) <:< (B ?=> C)]

}

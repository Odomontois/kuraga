object Main {

  def main(args: Array[String]): Unit = {
    println("Hello world!")
    println(msg)
  }

  def msg = "I was compiled by dotty :)"

  // trait A
  // trait B extends A

  // the[((x : A) => Int) <:< ((x: B) => Int )]

  def check[A,  B <: A, C] = the[(given A => C) <:< (given B => C)]

}

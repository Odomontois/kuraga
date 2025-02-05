package playground.chat.macromatch



@main def myMain(): Unit = {
  val anyF: Any => Boolean       = _ => true
  val stringF: String => Boolean = _ => true
  val matched =
    MacroTest.checkMatch(
      Option("exists").exists(_ => true),
      Option("exists with anyF").exists(anyF),
      Option("exists with stringF").exists(stringF)
    )

  println(matched.mkString("\n"))
}
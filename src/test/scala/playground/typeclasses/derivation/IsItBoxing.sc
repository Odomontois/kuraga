object CharType{
  type Symbol = 1
  val Symbol: Symbol = 1
  type NewLine = 2
  val NewLine: NewLine = 2
  type Separator = 3
  val Separator: Separator = 3

  opaque type CharType = Symbol | NewLine | Separator

  def isItBoxing(x: Symbol): Symbol = x

}

CharType.getClass.getMethods.filter(_.getName == "isItBoxing").head

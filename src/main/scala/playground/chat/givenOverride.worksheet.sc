trait A:
  // val strToOverride = "42"
  // given str: String = strToOverride
    given str: String = "42"

class B extends A:
  // override val strToOverride = "42B"
    override def str: String = "42B"
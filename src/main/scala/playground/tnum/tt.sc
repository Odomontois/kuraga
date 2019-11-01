val ho = [A] => (xs: List[A]) => xs.headOption
  

  ho(List(1, 2, 3))
  ho(Nil)
  ho(List("lol", "kek"))
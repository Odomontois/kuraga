package kuraga.datum

class StarSuite extends munit.FunSuite:
  test("result") {
    assertEquals(Star.Result(1).run, 1)
  }

  test("delayed result") {
    assertEquals(Star.Result(1).delay.run, 1)
  }

  test("long delay result") {
    def go(r: Star[AnyP, Int], rest: Long): Star[AnyP, Int] =
      if rest == 0 then r else r.delay

    assertEquals(go(Star.Result(1), 100000).run, 1)
  }
end StarSuite

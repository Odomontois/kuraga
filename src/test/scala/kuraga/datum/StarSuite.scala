package kuraga.datum

import kuraga.datum.Result.Result

class StarSuite extends munit.FunSuite:
  test("result") {
    assertEquals(Star.Done(1).run, 1)
  }

  test("delayed result") {
    assertEquals(Star.Done(1).delay.run, 1)
  }

  test("long delay result") {
    def go(r: Star[AnyP, Int], rest: Long): Star[AnyP, Int] =
      if rest == 0 then r else r.delay

    assertEquals(go(Star.Done(1), 100000).run, 1)
  }

  test("flatMap is oq") {

    val x = Result.pure[Int](1)

    val y = x.flatMap(x => Result.pure(x + 1))

    val yres = y.elimRun(Result.handler(x => Star.Done(x)))

    assertEquals(yres, 2)
  }
end StarSuite

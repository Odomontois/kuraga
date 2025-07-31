trait Dumb[A]

trait Dumber[A] extends Dumb[A]

trait Momo:
    def lol[X](using Dumber[X]): X

trait Dede extends Momo:
    override def lol[X: Dumber]: X


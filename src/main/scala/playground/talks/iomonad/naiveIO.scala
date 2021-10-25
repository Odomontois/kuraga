package playground.talks.iomonad

import playground.talks.iomonad.NaiveIO.IORef

trait NaiveIO[+A]:
  import NaiveIO.*

  def run(world: World): (A, World)

  def flatMap[B](f: A => NaiveIO[B]): NaiveIO[B] = world1 =>
    val (a, world2) = run(world1)
    f(a).run(world2)

  def map[B](f: A => B): NaiveIO[B] = world1 =>
    val (a, world2) = run(world1)
    (f(a), world1)

  def unsafeRun() = run(new World)._1

object NaiveIO:
  def apply[A](a: => A): NaiveIO[A] = world => (a, world)

  class World private[NaiveIO] ()

  abstract class IORef[A] private[NaiveIO]:
    def set(a: A): NaiveIO[Unit]
    def get: NaiveIO[A]
    def update(f: A => A): NaiveIO[Unit] = get.flatMap(x => set(f(x)))

  def newIORef[A](a: A): NaiveIO[IORef[A]] = NaiveIO {
    new:
      var state     = a
      def set(a: A) = NaiveIO { state = a }
      def get       = NaiveIO(state)
  }

def countZeros(xs: LazyList[Int]): NaiveIO[Int] =
  def go(xs: LazyList[Int], ref: IORef[Int]): NaiveIO[Unit] = xs match
    case 0 #:: rest => ref.update(_ + 1).flatMap(_ => go(rest, ref))
    case _ #:: rest => go(rest, ref)
    case _          => NaiveIO(())
  for
    ref <- NaiveIO.newIORef(0)
    _   <- go(xs, ref)
    x   <- ref.get
  yield x

@main def foo =
  println(countZeros(LazyList.fill(10000)(0)).unsafeRun())

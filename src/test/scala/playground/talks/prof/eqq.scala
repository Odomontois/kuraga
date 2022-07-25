package playground.talks.prof.eqq

import kuraga.Functor

def fromPair[A](p: (A, A)) = (b: Boolean) => if (b) p._1 else p._2

def toPair[A](f: Boolean => A) = (f(true), f(false))

def capture[A](a: A) = () => a

def eval[A](t: () => A) = t()

trait Lazy[A]:
  def use[B](f: A => B): B

def toLazy[A](a: A): Lazy[A] = new:
  def use[B](f: A => B) = f(a)

def fromLazy[A](l: Lazy[A]): A = l.use(identity)



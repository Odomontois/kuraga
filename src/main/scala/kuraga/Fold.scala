package kuraga
import Eval.delay

trait Fold[C, A]:
    def [B] (c: C) foldMap (f: A => Eval[B])(using Monoid[B]): Eval[B]

    def [B] (c: C) foldMapE (f: A => B)(using Monoid[B]): B = c.foldMap(a => f(a).delay).value
    
    def [B] (c: C) foldr (lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = 
        c.foldMap(a => Eval.now(Endo[B](f(a, _)))).value(lb)
    
    def [B] (c: C) foldl (b: B)(f: (B, A) => B): B = c.foldMapE(a => EndoE[B](f(_, a))).run(b)

trait Foldable[C[_]] extends Forall[[A] =>> Fold[C[A], A]]:
    self =>
    def [A, B](c: C[A]) foldMapA (f: A => Eval[B])(using Monoid[B]): Eval[B]

    def of[A] = new {
        def [B] (c: C[A]) foldMap (f: A => Eval[B])(using Monoid[B]): Eval[B] = c.foldMapA(f)
    }


trait Reduce[C, A] extends Fold[C, A]:
    def [B] (c: C) reduceMap(f: A => Eval[B])(using Semigroup[B]): Eval[B]
    def [B] (c: C) foldMap(f: A => Eval[B])(using Monoid[B]): Eval[B] = c.reduceMap(f)

trait Reducible[C[_]] extends Forall[[A] =>> Reduce[C[A], A]] with Foldable[C]  :
    self =>
    def [A, B] (c: C[A]) reduceMapA (f: A => Eval[B])(using Semigroup[B]): Eval[B]
    def [A, B](c: C[A]) foldMapA (f: A => Eval[B])(using Monoid[B]): Eval[B] = c.reduceMapA(f)
 
    override def of[A] = new {
        def [B] (c: C[A]) reduceMap (f: A => Eval[B])(using Semigroup[B]): Eval[B] = c.reduceMapA(f)
    }


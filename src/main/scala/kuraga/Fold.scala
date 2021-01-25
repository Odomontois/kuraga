package kuraga
import Eval.delay

trait Fold[C, A]:
    extension [B](c: C)
        def foldMap (f: A => Eval[B])(using Monoid[B]): Eval[B]
        def foldMapE (f: A => B)(using Monoid[B]): B  = c.foldMap(a => Eval.later(f(a))).value   
        def foldr (lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = 
            this.foldr(c)(lb)(f)        
        def foldl (b: B)(f: (B, A) => B): B = c.foldMapE(a => EndoE[B](f(_, a))).run(b)

trait Foldable[C[_]] extends Forall[[A] =>> Fold[C[A], A]]:
    self =>
    extension [A, B](c: C[A])
        def foldMapA (f: A => Eval[B])(using Monoid[B]): Eval[B]

    def of[A] = new {
        extension [B] (c: C[A]) def foldMap(f: A => Eval[B])(using Monoid[B]): Eval[B] = c.foldMapA(f)
    }


trait Reduce[C, A] extends Fold[C, A]:
    extension[B](c: C)
        def reduceMap(f: A => Eval[B])(using Semigroup[B]): Eval[B]
        def foldMap(f: A => Eval[B])(using Monoid[B]): Eval[B] = this.reduceMap(c)(f)

trait Reducible[C[_]] extends Forall[[A] =>> Reduce[C[A], A]] with Foldable[C]  :
    self =>
    extension[A, B](c: C[A])
        def reduceMapA (f: A => Eval[B])(using Semigroup[B]): Eval[B] 
        def foldMapA (f: A => Eval[B])(using Monoid[B]): Eval[B] = this.reduceMapA(c)(f)
 
    override def of[A] = new {
        extension [B] (c: C[A]) def reduceMap (f: A => Eval[B])(using Semigroup[B]): Eval[B] = c.reduceMapA(f)
    }


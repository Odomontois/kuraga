package kuraga

trait Fold[C, A]
    def [B] (c: C) foldMap (f: A => Eval[B])(given Monoid[B]): Eval[B]

    def [B] (c: C) foldMapE (f: A => B)(given Monoid[B]): B = c.foldMap(a => Eval.delay(f(a))).value
    
    def [B] (c: C) foldr (lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = 
        c.foldMap(a => Eval.now(Endo[B](f(a, _)))).value(lb)
    
    def [B] (c: C) foldl (b: B)(f: (B, A) => B): B = c.foldMapE(a => EndoE[B](f(_, a))).run(b)

trait Foldable[C[_]] extends Forall[[A] =>> Fold[C[A], A]]

trait Reduce[C, A] extends Fold[C, A]
    def [B] (c: C) reduceMap(f: A => Eval[B])(given Semigroup[B]): Eval[B]
    def [B] (c: C) foldMap(f: A => Eval[B])(given Monoid[B]): Eval[B] = c.reduceMap(f)

trait Reducible[C[_]] extends Forall[[A] =>> Reduce[C[A], A]]

object Reduce
    given [A] : Reduce[Id[A], A]
        def [B] (c: Id[A]) foldMap (f: A => Eval[B])(given Semigroup[B]): Eval[B] = f(c.get)


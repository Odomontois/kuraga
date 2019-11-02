package kuraga

trait Folding[C, A]{
    def [B] (c: C) foldMap (f: A => Eval[B])(given Monoid[B]): Eval[B]
    
    def [B] (c: C) foldr (lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = 
        c.foldMap(a => Eval.now(Endo[B](f(a, _)))).value(lb)
    
    // def [B] (c: C) foldl (b: B)(f: (B, A) => B): B = 
    //     c.foldMap()
}
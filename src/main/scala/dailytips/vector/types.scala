package dailytips.vector

type Coll[+A] = Vector[A]

def append[A](c: Coll[A], a: A): Coll[A]  = c :+ a
def prepend[A](c: Coll[A], a: A): Coll[A] = a +: c

def concat[A](c1: Coll[A], c2: Coll[A]): Coll[A] = c1 ++ c2
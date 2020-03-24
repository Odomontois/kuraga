def f[A <: Int & Singleton](x: A) (using A: ValueOf[A]): A = 
    A.value

val z = f(1)




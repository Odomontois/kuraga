def f[A <: Int & Singleton](x: A) (given A: ValueOf[A]): A = 
    A.value

val z = f(1)




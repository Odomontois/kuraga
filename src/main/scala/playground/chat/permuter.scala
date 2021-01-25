package playground.chat

object Permuter:
    def permute(n: Int, a: Array[Char]): Unit = 
        if(n == 0) println(a.mkString)
        else for i <- 1 to n do 
            permute(n - 1, a)
            a.swap(i * (n % 2), n)   
            
    extension (a: Array[Char]) def swap (i: Int, j: Int): Unit = 
        val r = a(i)
        a(i) = a(j)
        a(j) = r
        

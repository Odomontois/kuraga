package kuraga
package lolcheck {

    import scala.compiletime._

    inline def tupleValues[A <: Tuple]: A = 
        inline erasedValue[A] match
            case _ : (t *: ts) => 
                summonInline[t *: ts <:< A](valueOf[t] *: tupleValues[ts])
            case _ : EmptyTuple => 
                summonInline[EmptyTuple <:< A](EmptyTuple)

    @main def lol = 
        val x = tupleValues[("Hello", 2, "world")]
        val x1: "Hello" = x._1
        println(x)
}

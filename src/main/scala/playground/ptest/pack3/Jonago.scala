package playground.ptest.pack3
import playground.ptest.pack1.TC
import playground.ptest.pack2.Mestillo

@main def jonago = 
    val x = summon[TC[Mestillo]].name
    println(x)
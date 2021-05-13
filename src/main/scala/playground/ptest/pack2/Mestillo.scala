package playground.ptest.pack2
import playground.ptest.pack1.TC

final case class Mestillo(x: String)
given TC[Mestillo] = TC.derived
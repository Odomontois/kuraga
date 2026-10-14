package playground

@main def hello() =
    val tt = playground.whatapackage
    println:
        tt.getClass.getDeclaredMethods.map(_.getName).mkString(" ")

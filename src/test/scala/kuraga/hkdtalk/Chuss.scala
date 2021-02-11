package kuraga
package hkdtalk
// import _root_.kuraga.syntax.{AppZipping, zipA}



@main def chuss = 
    val zaks = (List(1, 2), List("o", "p"), List(true, false))
    // summon[AppZipping.AuxB[EmptyTuple, List]]
    // summon[AppZipping[(List[Int], List[String], List[Boolean])]]
    // AppZipping.instance[List, (List[Int], List[String], List[Boolean])]
    // val z = summon[AppZipping[List, (List[Int], List[String], List[Boolean])]]
    val zz = (1, "lol", true)
    // val kerb = zaks.zipA
    // println(kerb)
    // for(k <- kerb) {
    //     println(s"${k._1} ${k._2} ${k._3}")
    // }

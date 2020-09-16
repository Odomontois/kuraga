package playground.chat.struc

type Person = {
    def id: String
    def name: String
    def isFriend(p: String) : Boolean
}

val oleg : Person = new {
    val id = "odomontois"
    val name = "Oleg"
    def isFriend(p: String) = false

    override def toString = s"I'm $name"
}

@main def strucGo =  println(oleg)
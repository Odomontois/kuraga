package playground.chat

opaque type Hash <: Long = Long

case class UserName[A](name: String)
case class Password[A](hash: Hash)

def help(id: UserName[String] | Password[Int]) =
  val user = id match
    case UserName(name) => lookupName(name)
    case Password(hash) => lookupPassword(hash)

def lookupName(name: String)   = Some(name)
def lookupPassword(hash: Hash) = Some(hash.toString)

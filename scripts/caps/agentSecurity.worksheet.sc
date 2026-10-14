//> using lib "io.circe::circe-core:0.14.16"

import io.circe.Json
import scala.compiletime.testing.typeCheckErrors
import scala.caps.SharedCapability
import scala.compiletime.Erased

type SecureJson = {
  def arr(json: Json*): Json
  def fromInt(int: Int): Json
  def fromString(string: String): Json
}

abstract class isCirceSecure(Json: SecureJson)
object CirceIsSecure extends isCirceSecure(io.circe.Json)

Json.arr(Json.fromInt(1), Json.fromString("2"))

class Filesystem extends SharedCapability
class Network    extends SharedCapability

object Good {
  def write(content: String, file: String)(using Filesystem): Unit = ()
}

object Bad {
  def write(content: String, file: String)(using Filesystem, Network): Unit = ()
}

type SecureFileOps = {
  def write(content: String, file: String)(using Filesystem): Unit
}

abstract class isFileOpsSecure(ops: SecureFileOps)
object GoodFileOpsAreSecure extends isFileOpsSecure(Good)

typeCheckErrors("object BadFileOpsAreSecure extends isFileOpsSecure(Bad)").head.message

//> using dep io.circe::circe-core:0.14.10
//> using dep io.circe::circe-parser:0.14.10
//> using dep io.circe::circe-generic:0.14.10

import io.circe._
import io.circe.derivation.{ConfiguredCodec, Configuration}
import io.circe.parser.decode

object Child {
    given Configuration = Configuration.default.withDiscriminator("kind")
    given Codec[Child] = ConfiguredCodec.derived[Child]
}

enum Child {
    case Son(name: String)
    case Daughter(name: String)
    case StepSon(name: String)
    case StepDaughter(name: String)
}

extension [A: Encoder as e](x: A) def asJson: String = e(x).noSpaces
def toJson[A: Encoder as e](x: A): String = e(x).noSpaces

import Child.{Son, Daughter}

val x = Son("a")
println:
    s"""
    ${x.asJson}

    ${ toJson(Son("Bob")) } -- compiles

    { Son("Bob").asJson } -- doesn't compile

    ${List(Son("Bob"), Daughter("Sarah")).asJson}

    ${decode[Child]("""{"name":"Bob"}""")}

    ${decode[Child]("""{"kind": "StepSon", "name":"Bob"}""")}
"""

package pureconfig.generic.scala3

import pureconfig.ConfigReader

import pureconfig._
import pureconfig.generic.semiauto._

case class Security(
    enabled: Boolean
)

case class Conf(
    security: Option[Security]
)

@main def lol() =
    given ConfigReader[Conf] = deriveReader
    // given ConfigReader[Security] = deriveReader

    given ConfigReader[Option[Security]] =
        HintsAwareConfigReaderDerivation.deriveReader[Option[Security]]

    println:
        ConfigSource
            .string:
                """{
                    security {
                        enabled = true
                    }
                }"""
            .loadOrThrow[Conf]

end lol

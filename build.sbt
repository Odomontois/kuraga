val dottyVersion = "0.14.0-RC1"


name := "dotty-simple"
version := "0.1.0"

scalaVersion := dottyVersion

libraryDependencies += ("org.typelevel" %% "cats-core" % "1.6.0").withSources().withDottyCompat(scalaVersion.value)
libraryDependencies += ("org.typelevel" %% "cats-effect" % "1.1.0").withSources().withDottyCompat(scalaVersion.value)


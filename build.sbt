val dottyVersion = "0.18.1-RC1"


name := "dotty-simple"
version := "0.1.0"

scalaVersion := dottyVersion

libraryDependencies += ("org.typelevel" %% "cats-core" % "2.0.0-RC1").withSources().withDottyCompat(scalaVersion.value)
libraryDependencies += ("org.typelevel" %% "cats-effect" % "2.0.0-RC1").withSources().withDottyCompat(scalaVersion.value)


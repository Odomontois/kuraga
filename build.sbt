val dottyVersion = "0.20.0-bin-20191029-d56060d-NIGHTLY"


name := "dotty-simple"
version := "0.1.0"

scalaVersion := dottyVersion

libraryDependencies += ("org.typelevel" %% "cats-core" % "2.0.0").withSources().withDottyCompat(scalaVersion.value)
libraryDependencies += ("org.typelevel" %% "cats-effect" % "2.0.0").withSources().withDottyCompat(scalaVersion.value)

scalacOptions += "-language:implicitConversions"


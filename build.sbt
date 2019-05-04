val dottyVersion = "0.14.0-RC1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "dotty-simple",
    version := "0.1.0",

    scalaVersion := dottyVersion,

    libraryDependencies += ("org.typelevel" %% "cats-core" % "1.6.0").withSources().withDottyCompat(scalaVersion.value)
  )

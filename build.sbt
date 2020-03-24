val dottyVersion = "0.23.0-RC1"


name := "dotty-simple"
version := "0.1.0"

scalaVersion := dottyVersion

libraryDependencies += ("org.typelevel" %% "cats-core" % "2.0.0").withSources().withDottyCompat(scalaVersion.value)
libraryDependencies += ("org.typelevel" %% "cats-free" % "2.0.0").withSources().withDottyCompat(scalaVersion.value)
libraryDependencies += ("org.typelevel" %% "cats-effect" % "2.0.0").withSources().withDottyCompat(scalaVersion.value)
libraryDependencies += ("ru.tinkoff"    %% "tofu-optics-core" % "0.6.1").withSources().withDottyCompat(scalaVersion.value)

scalacOptions += "-language:implicitConversions"
scalacOptions += "-Ykind-projector"


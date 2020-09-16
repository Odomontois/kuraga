val dottyVersion = "0.27.0-RC1"


name := "kuraga"
version := "0.1.0"

scalaVersion := dottyVersion

libraryDependencies += ("org.typelevel" %% "cats-core"   % "2.0.0").withSources().withDottyCompat(scalaVersion.value)
libraryDependencies += ("org.typelevel" %% "cats-free"   % "2.0.0").withSources().withDottyCompat(scalaVersion.value)
libraryDependencies += ("org.typelevel" %% "cats-effect" % "2.0.0").withSources().withDottyCompat(scalaVersion.value)
libraryDependencies += ("ru.tinkoff"    %% "tofu"        % "0.7.3").withSources().withDottyCompat(scalaVersion.value)
libraryDependencies += ("com.softwaremill.sttp.client" %% "core" % "2.2.0").withSources().withDottyCompat(scalaVersion.value)

scalacOptions += "-language:implicitConversions"
scalacOptions += "-Ykind-projector"


val dottyVersion = "3.0.0"

name := "kuraga"
version := "0.1.0"

scalaVersion := dottyVersion

libraryDependencies += ("org.typelevel"                 %% "cats-core"   % "2.3.1").cross(CrossVersion.for3Use2_13)
libraryDependencies += ("org.typelevel"                 %% "cats-free"   % "2.3.1").cross(CrossVersion.for3Use2_13)
libraryDependencies += ("org.typelevel"                 %% "cats-effect" % "2.3.1").cross(CrossVersion.for3Use2_13)
libraryDependencies += ("ru.tinkoff"                    %% "tofu"        % "0.9.0").cross(CrossVersion.for3Use2_13)
libraryDependencies += ("com.softwaremill.sttp.client3" %% "core"        % "3.0.0-RC13")
  .withSources()
  .cross(CrossVersion.for3Use2_13)

scalacOptions += "-language:implicitConversions"
scalacOptions += "-Ykind-projector"

scalacOptions ++= Vector("-Xmax-inlines", "200")
// scalacOptions ++= Vector("-source", "future-migration", "-rewrite", "-deprecation", "rewrite")

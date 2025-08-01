val dottyVersion = "3.7.2"
name    := "kuraga"
version := "0.1.0"

scalaVersion := dottyVersion

libraryDependencies += ("org.typelevel" %% "cats-core"   % "2.7.0")
libraryDependencies += ("org.typelevel" %% "cats-free"   % "2.7.0")
libraryDependencies += ("org.typelevel" %% "cats-effect" % "3.3.12")
//libraryDependencies += ("tf.tofu"       %% "tofu-core-ce3" % "0.10.8").cross(CrossVersion.for3Use2_13)

libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test

libraryDependencies += "org.typelevel" %% "shapeless3-typeable" % "3.1.0"

libraryDependencies += "org.typelevel" %% "spire" % "0.18.0"

scalacOptions += "-language:implicitConversions"
scalacOptions += "-Ykind-projector"
scalacOptions += "-explain"
scalacOptions += "-Yexplicit-nulls"
scalacOptions ++= Vector("-Xmax-inlines", "1000")
// scalacOptions ++= Vector("-source", "future-migration", "-rewrite", "-deprecation", "rewrite")

Compile / doc / scalacOptions ++= Vector("-siteroot", "docs")

Compile / doc / target := file("site")

lazy val cap = project.settings(
  scalaVersion := dottyVersion,
  scalacOptions ++= Vector(
    "-experimental",
    "-language:experimental.captureChecking",
    "-explain",
    // "-Ycc-debug",
  ),
)

lazy val root = project.in(file(".")).aggregate(cap)

val dottyVersion  = "3.7.4-RC1"
val stableVersion = "3.7.3"
name    := "kuraga"
version := "0.1.0"

scalaVersion := stableVersion

val kyoVersion       = "1.0-RC1"
val mijnInstellingen = Seq(
  libraryDependencies += ("org.typelevel" %% "cats-core"   % "2.7.0"),
  libraryDependencies += ("org.typelevel" %% "cats-free"   % "2.7.0"),
  libraryDependencies += ("org.typelevel" %% "cats-effect" % "3.3.12"),
  libraryDependencies += "com.github.pureconfig" %% "pureconfig-core"           % "0.17.9",
  libraryDependencies += "com.github.pureconfig" %% "pureconfig-generic-scala3" % "0.17.9",
  libraryDependencies += "io.getkyo"             %% "kyo-prelude"               % kyoVersion,
  libraryDependencies += "io.getkyo"             %% "kyo-core"                  % kyoVersion,
  libraryDependencies += "org.scalameta"         %% "munit"                     % "0.7.29" % Test,
  libraryDependencies += "org.typelevel"         %% "shapeless3-typeable"       % "3.1.0",
  libraryDependencies += "org.typelevel"         %% "spire"                     % "0.18.0",
  scalacOptions += "-language:implicitConversions",
  scalacOptions += "-Xkind-projector",
  scalacOptions += "-explain",
  scalacOptions += "-Yexplicit-nulls",
  scalacOptions ++= Vector("-Xmax-inlines", "1000"),
  scalacOptions += "-language:experimental.modularity",
  Compile / doc / scalacOptions ++= Vector("-siteroot", "docs"),
  Compile / doc / target                         := file("site"),
)

lazy val cap = project.settings(
  scalaVersion                                   := dottyVersion,
  scalacOptions ++= Vector(
    "-experimental",
    "-language:experimental.captureChecking",
    "-explain",
    "-Wunused:imports,privates,locals,implicits",
    "-language:experimental.saferExceptions"
    // "-Ycc-debug",
  ),
  libraryDependencies += "software.amazon.smithy" % "smithy-model" % "1.61.0",
  libraryDependencies += ("org.typelevel" %% "cats-core" % "2.7.0"),
)

ThisBuild / resolvers += Resolver.scalaNightlyRepository

lazy val root = project.in(file(".")).settings(mijnInstellingen).aggregate(cap)

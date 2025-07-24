val scala3Version = "3.7.1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "adventofcode2016-scala3",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,

    // required for use with Metals for navigation in external dependencies to work
    bloopExportJarClassifiers in Global := Some(Set("sources")),

    libraryDependencies += "dev.zio" %% "zio" % "2.1.20",
    libraryDependencies += "dev.zio" %% "zio-logging" % "2.5.1",
    libraryDependencies += "dev.zio" %% "zio-nio" % "2.0.2",
    libraryDependencies += "org.scalameta" %% "munit" % "1.1.1" % Test,
    libraryDependencies += "dev.zio" %% "zio-test" % "2.1.20" % Test
  )

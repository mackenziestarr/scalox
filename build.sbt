val scala3Version = "3.0.1"

val deps = List(
  "org.scalatest" %% "scalatest" % "3.2.10" % Test,
)

lazy val root = project
  .in(file("."))
  .settings(
    name := "lox",
    version := "0.1.0",
    scalaVersion := scala3Version,
    libraryDependencies ++= deps,
  )

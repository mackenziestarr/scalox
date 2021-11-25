lazy val root = project
  .in(file("."))
  .settings(
    name := "lox",
    version := "0.1.0",
    scalaVersion := "3.0.1",
    libraryDependencies ++= List(
      "org.typelevel" %% "cats-core" % "2.6.1",
      "org.scalatest" %% "scalatest" % "3.2.10" % Test,
    ),
  )
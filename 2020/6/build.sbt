val dottyVersion = "0.27.0-RC1"

Global / onChangedBuildSource := ReloadOnSourceChanges

lazy val root = project
  .in(file("."))
  .settings(
    name := "6",
    version := "0.1.0",

    scalaVersion := dottyVersion,
    scalacOptions +=  "-Yindent-colons",

    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.2" % Test,
    libraryDependencies += "com.github.j-mie6" %% "parsley" % "1.5.0"

  )

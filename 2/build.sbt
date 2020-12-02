val dottyVersion = "0.27.0-RC1"

Global / onChangedBuildSource := ReloadOnSourceChanges

lazy val parsley = RootProject(uri("git://github.com/j-mie6/parsley.git"))

lazy val root = project
  .in(file("."))
  .dependsOn(parsley)
  .settings(
    name := "2",
    version := "0.1.0",

    scalaVersion := dottyVersion,
    scalacOptions +=  "-Yindent-colons",

    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.2" % Test

  )

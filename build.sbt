ThisBuild / scalaVersion     := "3.1.2"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.example"
ThisBuild / organizationName := "example"

lazy val root = (project in file("."))
  .settings(
    name := "SpiralMatrix",
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio" % "2.0.0-RC6"
    )
  )

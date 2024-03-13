ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.1"

lazy val root = (project in file("."))
  .settings(
    name := "Conways Game Of Life" ,
    libraryDependencies += "org.creativescala" %% "doodle" % "0.21.0"
  )

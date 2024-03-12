ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.1.3"

lazy val root = (project in file("."))
  .settings(
    name := "unnamed" ,
    libraryDependencies += "org.creativescala" %% "doodle" % "0.21.0"
  )

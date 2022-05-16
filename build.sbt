ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.1.2"

lazy val root = (project in file("."))
  .settings(
    name := "eika",
    idePackagePrefix := Some("ink.sora")
  )

libraryDependencies += "org.scala-lang" % "scala-reflect" % "2.13.8"
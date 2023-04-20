ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.0.2"

lazy val root = (project in file("."))
  .settings(
    name := "Scala1"
  )
libraryDependencies ++= Seq(

  "com.typesafe" % "config" % "1.4.2"

)
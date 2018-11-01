import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.example",
      scalaVersion := "2.12.7",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "restest",
    libraryDependencies ++= Seq(
      "org.apache.httpcomponents" % "httpclient" % "4.5.2",
      "com.fasterxml.jackson.core" % "jackson-core" % "2.8.1",
      "com.fasterxml.jackson.core" % "jackson-databind" % "2.8.1",
      scalaTest % Test
    )
  )

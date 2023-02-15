ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.1.3"

lazy val root = (project in file("."))
  .settings(
    name := "Scrapper"
  )

libraryDependencies += "org.jsoup" % "jsoup" % "1.15.3"

ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.1.3"
//ThisBuild / scalaVersion := "2.13.10"

lazy val root = (project in file("."))
  .settings(
    name := "Scrapper"
  )

libraryDependencies += "org.jsoup" % "jsoup" % "1.15.3"
libraryDependencies += "com.github.tototoshi" %% "scala-csv" % "1.3.8"
/*libraryDependencies += "org.scalaj" %% "scalaj-http" % "2.4.2"
libraryDependencies += "org.seleniumhq.selenium" % "selenium-java" % "4.1.1"*/

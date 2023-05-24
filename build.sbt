ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.1.3"
//ThisBuild / scalaVersion := "2.13.10"

lazy val root = (project in file("."))
  .settings(
    name := "Scrapper"
  )

//do not reload sbt as there are some maven dependencies added:
//mongodb.scala.mongo.driver_2.13
//slf4j.simple

//libraryDependencies += "org.mongodb" % "mongo-csv-driver" % "1.10.3"

libraryDependencies += "org.mongodb" % "mongo-java-driver" % "3.12.12"

libraryDependencies += "org.jsoup" % "jsoup" % "1.15.3"
libraryDependencies += "com.github.tototoshi" %% "scala-csv" % "1.3.8"

/*//works only for 2.13 Scala version
libraryDependencies += "org.mongodb.scala" %% "mongo-scala-driver" % "4.9.1"*/

/*libraryDependencies += "org.scalaj" %% "scalaj-http" % "2.4.2"
libraryDependencies += "org.seleniumhq.selenium" % "selenium-java" % "4.1.1"*/

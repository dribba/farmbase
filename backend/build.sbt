name := """formbase"""

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "2.11.7"

val mongoDriver = "org.mongodb.scala" %% "mongo-scala-driver" % "1.1.1"

libraryDependencies ++= Seq(
  jdbc,
  cache,
  ws,
  "org.scalaz" %% "scalaz-core" % "7.2.8",
  "org.scalatestplus.play" %% "scalatestplus-play" % "1.5.1" % Test,
  mongoDriver
)


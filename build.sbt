name := """akka-scala-seed"""

version := "1.0"

scalaVersion := "2.10.4"

libraryDependencies ++= Seq(
  "org.scalactic" %% "scalactic" % "3.0.0",
  // Change this to another test framework if you prefer
  "org.scalatest" %% "scalatest" % "3.0.0" % "test",
  // Akka
  "com.typesafe.akka" %% "akka-actor" % "2.3.5"
  //"com.typesafe.akka" %% "akka-remote" % "2.3.5",
  //"com.typesafe.akka" %% "akka-testkit" % "2.3.5"
)

name := "mcts"

version := "1.2.0"

scalaVersion := "2.11.8"

// ~~~ ScalaTest
libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.4"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.4" % "test"
libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.13.4" % "test"

// ~~~ Scallop, used for command line parsing in example App
libraryDependencies += "org.rogach" %% "scallop" % "3.1.0"


libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.2.25"
libraryDependencies += "org.scalaz" %% "scalaz-effect" % "7.2.25"

scalacOptions ++= Seq(
  "-unchecked",
  "-feature",
  "-language:reflectiveCalls",
  "-language:existentials",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-language:postfixOps",
  "-deprecation",
  "-encoding",
  "utf8"
)
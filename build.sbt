lazy val scala212 = "2.12.10"
lazy val scala211 = "2.11.12"
lazy val supportedScalaVersions = List("2.12.10", "2.11.12")

ThisBuild / organization := "cse.bdlab"
ThisBuild / version      := "1.4.0"
ThisBuild / scalaVersion := "2.12.10"

lazy val root = (project in file("."))
  .settings(
    crossScalaVersions := supportedScalaVersions,
    name := "mcts",
    libraryDependencies := deps,
    scalacOptions := compilerOps
  )

lazy val deps = Seq(

  // ~~~ ScalaTest
  "org.scalactic" %% "scalactic" % "3.0.4",
  "org.scalatest" %% "scalatest" % "3.0.4" % "test",
  "org.scalacheck" %% "scalacheck" % "1.13.4" % "test",

  // ~~~ Scallop, used for command line parsing in example App
  "org.rogach" %% "scallop" % "3.1.0",

  // ~~~ Cats FP Library
  "org.typelevel" %% "cats-core" % "2.1.1"

)

lazy val compilerOps = Seq(
  "-unchecked",
  "-feature",
  "-Ypartial-unification",
  "-language:reflectiveCalls",
  "-language:existentials",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-language:postfixOps",
  "-deprecation",
  "-encoding",
  "utf8"
)
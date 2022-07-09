lazy val scala213 = "2.13.6"
lazy val scala212 = "2.12.16"
lazy val scala211 = "2.11.12"
// lazy val supportedScalaVersions = List(scala213)

ThisBuild / organization := "cse.bdlab"
ThisBuild / version := "1.4.0"
ThisBuild / scalaVersion := scala213

lazy val root = (project in file("."))
  .settings(
    // crossScalaVersions := supportedScalaVersions,
    name := "mcts",
    libraryDependencies := deps
    // scalacOptions := compilerOps
  )

lazy val deps = Seq(
  // ~~~ ScalaTest
  // "org.scalactic"  %% "scalactic"  % "3.0.4",
  "org.scalatest"  %% "scalatest"  % "3.0.9"  % "test",
  "org.scalacheck" %% "scalacheck" % "1.14.0" % "test",
  // ~~~ Scallop, used for command line parsing in example App
  "org.rogach" %% "scallop" % "3.5.1",
  // ~~~ Cats FP Library
  "org.typelevel" %% "cats-core" % "2.3.0"
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

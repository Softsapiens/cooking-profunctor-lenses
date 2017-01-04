name := "algelab"

scalaVersion in ThisBuild := "2.12.1"

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3")
addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)


import sbt._

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats" % "0.8.1"
  , "org.scalaz" %% "scalaz-core" % "7.3.0-M7"
  , "org.scalatest" %% "scalatest" % "3.0.1" % Test
  , "com.lihaoyi" %% "sourcecode" % "0.1.3"
  , "com.lihaoyi" % "ammonite" % "COMMIT-3119490" % "test" cross CrossVersion.full
  , "io.monix" %% "monix" % "2.1.1"
  , "io.monix" % "monix-types_2.12" % "2.1.1"
  , "io.monix" % "monix-eval_2.12" % "2.1.1"
  , "io.monix" % "monix-cats_2.12" % "2.1.1"
  , "io.monix" %% "monix-scalaz-72" % "2.1.1"
  , "com.github.mpilquist" %% "simulacrum" % "0.10.0"
  , "com.github.julien-truffaut" %%  "monocle-core" % "1.4.0-M2"
)

scalacOptions ++= Seq(
  "-unchecked",
  "-deprecation",
  "-Ypartial-unification",
  //"-Ylog-classpath",
  // "-Xprint:typer",
  // "-Xlog-implicit-conversions",
  "-feature",
  "-language:implicitConversions",
  "-language:postfixOps",
  "-language:higherKinds")

initialCommands in console := """
  |import algelab._
  """.stripMargin

// initialCommands in (Test, console) := """ammonite.Main().run()"""

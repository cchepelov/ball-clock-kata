import sbt.Keys._

organization := "org.chepelov.flaminem.kata"
name := "ballclock-kata"
version := "0.0.1-SNAPSHOT"
description := "The Ball BallClock kata"

scalaVersion := "2.12.10"

scalacOptions ++= Seq(
  "-Xmacro-settings:materialize-derivations",
  // Enables compilation errors on non-exhaustive pattern matchings.
  // Use @silent annotations to consciously silent out warnings if needed
  // (https://github.com/ghik/silencer).
  "-Xfatal-warnings",
  "-deprecation",
)

val silencerVersion = "1.4.4"
val scalatestVersion = "3.1.0"
val zioVersion = "1.0.0-RC17"
val catsVersion = "2.0.0"

libraryDependencies ++= Seq(
  compilerPlugin("com.github.ghik" % "silencer-plugin" % silencerVersion cross CrossVersion.full),
  "com.github.ghik" % "silencer-lib" % silencerVersion % Provided cross CrossVersion.full
)


libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % catsVersion,

  "dev.zio" %% "zio" % zioVersion,
  "dev.zio" %% "zio-streams" % zioVersion,

  "com.typesafe.play" %% "play-json" % "2.8.1",
)

libraryDependencies ++= Seq(
    "org.scalactic" %% "scalactic" % scalatestVersion,
    "org.scalatest" %% "scalatest" % scalatestVersion % Test,
    "org.scalatestplus" %% "scalatestplus-scalacheck" % "3.1.0.0-RC2" % Test,
    "org.scalacheck" %% "scalacheck" % "1.14.2" % Test,
    "org.scalamock" %% "scalamock" % "4.4.0" % Test,
    "com.lihaoyi" %% "pprint" % "0.5.6" % Test,
)



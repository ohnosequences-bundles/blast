Nice.scalaProject

name := "blast"
organization := "ohnosequencesBundles"
description := "A bundle for blast tool"


publishBucketSuffix := "era7.com"

resolvers ++= Seq(
  "Era7 public maven releases"  at s3("releases.era7.com").toHttps(s3region.value.toString),
  "Era7 public maven snapshots" at s3("snapshots.era7.com").toHttps(s3region.value.toString)
)

libraryDependencies ++= Seq(
  "ohnosequences" %% "statika" % "2.0.0-SNAPSHOT",
  "org.scalatest" %% "scalatest" % "2.2.4" % Test,
  "com.lihaoyi"   %% "ammonite-ops" % "0.2.7"
)

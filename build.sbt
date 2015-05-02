
name := "blast"
organization := "ohnosequences-bundles"
description := "A bundle for blast tool"

scalaVersion := "2.11.6"

publishBucketSuffix := "era7.com"

libraryDependencies ++= Seq(
  "ohnosequences" %% "statika" % "2.0.0-M1",
  "com.lihaoyi"   %% "ammonite-ops" % "0.2.7"
)

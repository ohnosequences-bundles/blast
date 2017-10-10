name         := "blast"
organization := "ohnosequences-bundles"
description  := "A bundle for blast tool"

publishBucketSuffix := "era7.com"

crossScalaVersions := Seq("2.11.11", "2.12.3")
scalaVersion := crossScalaVersions.value.max

libraryDependencies ++= Seq(
  "ohnosequences" %% "statika" % "3.0.0"
)

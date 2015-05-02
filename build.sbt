Nice.scalaProject

name := "blast"
organization := "ohnosequences-bundles"
description := "A bundle for blast tool"

scalaVersion := "2.11.6"

resolvers += "Era7 maven releases" at "https://s3-eu-west-1.amazonaws.com/releases.era7.com"

libraryDependencies += "ohnosequences" %% "statika" % "2.0.0-M1"

publishBucketSuffix := "era7.com"

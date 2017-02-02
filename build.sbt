name := "blast"
organization := "ohnosequences-bundles"
description := "A bundle for blast tool"

publishBucketSuffix := "era7.com"

resolvers += "Era7 public maven releases" at s3("releases.era7.com").toHttps(s3region.value.toString)

libraryDependencies += "ohnosequences" %% "statika" % "2.0.0"

wartremoverErrors in (Compile, compile) := Seq()

releaseOnlyTestTag := "ohnosequencesBundles.test.ReleaseOnlyTest"

name := "blast"
organization := "ohnosequences-bundles"
description := "A bundle for blast tool"

publishBucketSuffix := "era7.com"

releaseOnlyTestTag := "ohnosequencesBundles.test.ReleaseOnlyTest"

resolvers += "Era7 public maven releases" at s3("releases.era7.com").toHttps(s3region.value.toString)

libraryDependencies ++= Seq(
  "ohnosequences"         %% "statika"         % "2.0.0",
  "ohnosequences-bundles" %% "compressinglibs" % "0.5.0"
)

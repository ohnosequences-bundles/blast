package ohnosequencesBundles.statika

import ohnosequences.statika._
import java.io.File
import sys.process._

abstract class IgBlast(val version: String, val blast: Blast) extends Bundle(blast) { igblast =>

  val tarball = s"ncbi-igblast-${igblast.version}-x64-linux.tar.gz"
  val binsFolder = s"ncbi-igblast-${igblast.version}"
  val filesFolder = "igblast"

  lazy val igdata = new File(filesFolder)
  lazy val database     = new File(igdata, "database")
  lazy val internalData = new File(igdata, "internal_data")
  lazy val optionalFile = new File(igdata, "optional_file")

  // the env var for the internal data
  lazy val IGDATA = ("IGDATA", igdata.getCanonicalPath)

  val binaries = List(
    "igblastn",
    "igblastp"
  )

  lazy val downloadBinaries = cmd("aws")("s3", "cp", s"s3://resources.ohnosequences.com/igblast/${igblast.version}/${igblast.tarball}", ".")
  lazy val downloadFiles    = cmd("aws")("s3", "cp", "--recursive", s"s3://resources.ohnosequences.com/igblast/files/", filesFolder)

  lazy val extractTarball = cmd("tar")("-xvf", igblast.tarball)

  lazy val linkBinaries = binaries map { name =>
    cmd("ln")(
      "-s",
      new File(s"${igblast.binsFolder}/bin/${name}").getCanonicalPath,
      s"/usr/bin/${name}"
    )
  }

  final def instructions: AnyInstructions = {

    downloadBinaries -&-
    extractTarball -&-
    linkBinaries.foldLeft[AnyInstructions](Seq("echo", "linking BLAST binaries"))( _ -&- _ ) -&-
    downloadFiles -&-
    say(s"${bundleFullName} is installed")
  }

  /* These two command runners set the environment variable to the interal data location */
  def n(args: String*): ProcessBuilder = Process("igblastn" +: args, None, IGDATA)
  def p(args: String*): ProcessBuilder = Process("igblastp" +: args, None, IGDATA)
}

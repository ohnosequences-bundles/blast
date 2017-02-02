package ohnosequencesBundles.statika

import ohnosequences.statika._
import java.io.File

abstract class IgBlast(val version: String, val blast: Blast) extends Bundle(blast) { igblast =>

  val tarball = s"ncbi-igblast-${igblast.version}-x64-linux.tar.gz"
  val folder  = s"ncbi-igblast-${igblast.version}"

  val binaries = List(
    "igblastn",
    "igblastp"
  )

  lazy val downloadBinaries = cmd("aws")("s3", "cp", s"s3://resources.ohnosequences.com/igblast/${igblast.version}/${igblast.tarball}", ".")
  lazy val downloadFiles    = cmd("aws")("s3", "cp", "--recursive", s"s3://resources.ohnosequences.com/igblast/files/", "igblast")

  lazy val extractTarball = cmd("tar")("-xvf", igblast.tarball)

  lazy val linkBinaries = binaries map { name =>
    cmd("ln")(
      "-s",
      new File(s"${igblast.folder}/bin/${name}").getCanonicalPath,
      s"/usr/bin/${name}"
    )
  }

  final def instructions: AnyInstructions = {

    downloadBinaries -&-
    extractTarball -&-
    linkBinaries.foldLeft[AnyInstructions](Seq("echo", "linking BLAST binaries"))( _ -&- _ ) -&-
    downloadFiles -&-
    // export IGBLAST env var
    say(s"${bundleFullName} is installed")
  }
}

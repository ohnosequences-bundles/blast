
```scala
package ohnosequencesBundles.statika

import ohnosequences.statika._
import java.io.File

abstract class Blast(val version: String) extends Bundle(compressinglibs) { blast =>

  // 2.2.31
  val tarball = s"ncbi-blast-${blast.version}-x64-linux.tar.gz"
  val folder  = s"ncbi-blast-${blast.version}+"

  val binaries = List(
    "blastdb_aliastool",
    "blastdbcheck",
    "blastdbcmd",
    "blast_formatter",
    "blastn",
    "blastp",
    "blastx",
    "convert2blastmask",
    "deltablast",
    "dustmasker",
    "legacy_blast.pl",
    "makeblastdb",
    "makembindex",
    "makeprofiledb",
    "psiblast",
    "rpsblast",
    "rpstblastn",
    "segmasker",
    "tblastn",
    "tblastx",
    "update_blastdb.pl",
    "windowmasker"
  )

  lazy val getTarball = cmd("wget")(
    s"https://s3-eu-west-1.amazonaws.com/resources.ohnosequences.com/blast/${blast.version}/${blast.tarball}"
  )

  lazy val extractTarball = cmd("tar")("-xvf", blast.tarball)

  lazy val linkBinaries = binaries map { name =>
    cmd("ln")(
      "-s",
      new File(s"${blast.folder}/bin/${name}").getCanonicalPath,
      s"/usr/bin/${name}"
    )
  }

  final def instructions: AnyInstructions = {

    getTarball -&-
    extractTarball -&-
    linkBinaries.foldLeft[AnyInstructions](Seq("echo", "linking BLAST binaries"))( _ -&- _ ) -&-
    say(s"${bundleFullName} is installed")
  }
}

```




[main/scala/blast.scala]: blast.scala.md

```scala
package ohnosequencesBundles.statika

import ohnosequences.statika._, bundles._, instructions._
import java.io.File

abstract class Blast(val version: String) extends Bundle { blast =>

  private def workingDir: String = (new File("")).getCanonicalPath().toString

  // 2.2.30+
  val tarball = s"ncbi-blast-${blast.version}-x64-linux.tar.gz"
  val folder  = s"ncbi-blast-${blast.version}"

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

  lazy val getTarball = Seq(
    "wget",
    s"https://s3-eu-west-1.amazonaws.com/resources.ohnosequences.com/blast/${blast.version}/${blast.tarball}"
  )

  lazy val extractTarball = Seq(
    "tar",
    "-xvf",
    blast.tarball
  )

  lazy val linkBinaries = binaries map { cmd => Seq(
      "ln",
      "-s",
      s"${workingDir}/${blast.folder}/bin/${cmd}",
      s"/usr/bin/${cmd}"
    )
  }

  def install: Results = getTarball ->- extractTarball ->-
    linkBinaries.foldLeft[Results](Seq("echo", "linking BLAST binaries"))( (acc, cmd) => acc ->- cmd ) ->-
    success(s"${bundleFullName} installed")
}

object blastAPI {

  trait AnyBlastCommand {  val name: String  }
  abstract class BlastCommand(val name: String) extends AnyBlastCommand
  case object blastn        extends BlastCommand("blastn");   type blastn = blastn.type
  case object blastp        extends BlastCommand("blastp");   type blastp = blastp.type
  case object blastx        extends BlastCommand("blastp")
  case object tblastn       extends BlastCommand("blastp")
  case object tblastx       extends BlastCommand("blastp")
  case object makeblastdb   extends BlastCommand("makeblastdb")

  trait AnyBlastOption {  def toSeq: Seq[String]  }
  abstract class BlastOption(val toSeq: Seq[String]) extends AnyBlastOption
  case class numThreads(val number: Int)    extends BlastOption( Seq("-num_threads", s"${number}") )
  case class db(val file: File)             extends BlastOption( Seq("-db", file.getCanonicalPath().toString) )
  case class query(val file: File)          extends BlastOption( Seq("-query", file.getCanonicalPath().toString) )
  case class out(val file: File)            extends BlastOption( Seq("-out", file.getCanonicalPath().toString) )
  case class evalue(val number: Long)       extends BlastOption( Seq("-evalue", number.toString) )
  case class maxTargetSeqs(val number: Int) extends BlastOption( Seq("-max_target_seqs", s"${number}") )

  case class blastCmd(val cmd: AnyBlastCommand, val opts: List[AnyBlastOption])

  // TODO: something like this with HList-like cmd type etc
  implicit val numThreadsBlastn: numThreads optionOf blastn = optionOf()
  implicit val numThreadsBlastp: numThreads optionOf blastp = optionOf()
  case class optionOf[BO <: AnyBlastOption, BC <: AnyBlastCommand]()
}

```




[main/scala/blast.scala]: blast.scala.md
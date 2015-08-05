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

  sealed trait AnyBlastOption {  def toSeq: Seq[String]  }
  abstract class BlastOption(val toSeq: Seq[String]) extends AnyBlastOption
  case class numThreads(val number: Int)    extends BlastOption( Seq("-num_threads", s"${number}") )
  case class db(val file: File)             extends BlastOption( Seq("-db", file.getCanonicalPath().toString) )
  case class query(val file: File)          extends BlastOption( Seq("-query", file.getCanonicalPath().toString) )
  case class out(val file: File)            extends BlastOption( Seq("-out", file.getCanonicalPath().toString) )
  case class evalue(val number: Long)       extends BlastOption( Seq("-evalue", number.toString) )
  case class maxTargetSeqs(val number: Int) extends BlastOption( Seq("-max_target_seqs", s"${number}") )
  case object showGis                       extends BlastOption( Seq("-show_gis") )
  sealed trait BlastnTask extends AnyBlastOption
  case object megablastTask                 extends BlastOption( Seq("-task", "megablast") ) with BlastnTask
  case object dcMegablastTask               extends BlastOption( Seq("-task", "dc-megablast") ) with BlastnTask
  case object blastnTask                    extends BlastOption( Seq("-task", "blastn") ) with BlastnTask
  case object blastnShortTask               extends BlastOption( Seq("-task", "blastn-short") ) with BlastnTask
  case object rmblastnTask                  extends BlastOption( Seq("-task", "rmblastn") ) with BlastnTask

  case class outfmt(val number: Int)        extends BlastOption( Seq("-outfmt", s"${number}") )
  trait AnyOutputFormat extends AnyBlastOption { val code: Int }
  private def outfmtSeq(code: Int): Seq[String] = Seq("-outfmt", s"${code}")
  abstract class OutputFormat(val code: Int, val toSeq: Seq[String]) extends AnyOutputFormat
  case object pairwiseOutputFormat                  extends OutputFormat(0,outfmtSeq(0))
  case object queryAnchoredShowIdsOutputFormat      extends OutputFormat(1, outfmtSeq(1))
  case object queryAnchoredNoIdsOutputFormat        extends OutputFormat(2, outfmtSeq(2))
  case object flatQueryAnchoredShowIdsOutputFormat  extends OutputFormat(3, outfmtSeq(3))
  case object flatQueryAnchoredNoIdsOutputFormat    extends OutputFormat(4, outfmtSeq(4))
  case object XMLOutputFormat                       extends OutputFormat(5, outfmtSeq(5))
  case object TSVOutputFormat                       extends OutputFormat(6, outfmtSeq(6))
  case object TSVWithCommentsOutputFormat           extends OutputFormat(7, outfmtSeq(7))
  case object TextASN1OutputFormat                  extends OutputFormat(8, outfmtSeq(8))
  case object BinaryASN1OutputFormat                extends OutputFormat(9, outfmtSeq(9))
  case object CSVOutputFormat                       extends OutputFormat(10, outfmtSeq(10))
  case object BLASTArchiveASN1OutputFormat          extends OutputFormat(11, outfmtSeq(11))
  case object JSONSeqalignOutputFormat              extends OutputFormat(12, outfmtSeq(12))

  // TODO fields for TSV and CSV

  case class blastCmd(val cmd: AnyBlastCommand, val opts: List[AnyBlastOption]) {

    def toSeq: Seq[String] = Seq(cmd.name) ++ (opts flatMap { _.toSeq} ) 
  }

  // TODO: something like this with HList-like cmd type etc
  implicit val numThreadsBlastn: numThreads optionOf blastn = optionOf()
  implicit val numThreadsBlastp: numThreads optionOf blastp = optionOf()
  case class optionOf[BO <: AnyBlastOption, BC <: AnyBlastCommand]()
}

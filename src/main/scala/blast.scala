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

  import ohnosequences.cosas.properties._
  import ohnosequences.cosas.typeSets._

  trait AnyBlastCommand {  val name: String  }
  abstract class BlastCommand(val name: String) extends AnyBlastCommand
  case object blastn        extends BlastCommand("blastn");   type blastn = blastn.type
  case object blastp        extends BlastCommand("blastp");   type blastp = blastp.type
  case object blastx        extends BlastCommand("blastp")
  case object tblastn       extends BlastCommand("blastp")
  case object tblastx       extends BlastCommand("blastp")
  case object makeblastdb   extends BlastCommand("makeblastdb")

  sealed trait AnyBlastOption {

    type Commands <: AnyTypeSet.Of[AnyBlastCommand]
    // val commands: Commands

    def toSeq: Seq[String]
  }
  abstract class BlastOption(val toSeq: Seq[String]) extends AnyBlastOption

  case class numThreads(val number: Int)    extends BlastOption( Seq("-num_threads", s"${number}") )
  case class db(val file: File)             extends BlastOption( Seq("-db", file.getCanonicalPath().toString) )
  case class query(val file: File)          extends BlastOption( Seq("-query", file.getCanonicalPath().toString) )
  case class out(val file: File)            extends BlastOption( Seq("-out", file.getCanonicalPath().toString) )
  case class evalue(val number: Double)     extends BlastOption( Seq("-evalue", number.toString) )
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
  trait AnyOutputField {

    lazy val blastRep: String = this.toString
  }

  trait AnyOutputRecordFormat {

    type Fields <: AnyTypeSet.Of[out.AnyOutputField]
    val fields: Fields
  }

  case class OutputRecordFormat[Flds <: AnyTypeSet.Of[out.AnyOutputField]]( val fields: Flds) extends AnyOutputRecordFormat {

    type Fields = Flds
  }

  object out {

    trait AnyOutputField extends AnyProperty
    trait OutputField[V] extends AnyOutputField { type Raw = V }

    // means Query Seq-id
    case object qseqid    extends OutputField[String]         { val label = toString }
    // means Query GI
    case object qgi       extends OutputField[String]         { val label = toString }
    // means Query accesion
    case object qacc      extends OutputField[String]         { val label = toString }
    // means Query accesion.version
    case object qaccver   extends OutputField[Int]            { val label = toString }
    // means Query sequence length
    case object qlen      extends OutputField[Int]            { val label = toString }
    // means Subject Seq-id
    case object sseqid    extends OutputField[String]         { val label = toString }
    // means All subject Seq-id(s), separated by a ';'
    case object sallseqid extends OutputField[List[String]]   { val label = toString }
    // means Subject GI
    case object sgi       extends OutputField[String]         { val label = toString }
    // means All subject GIs
    case object sallgi    extends OutputField[List[String]]   { val label = toString }
    // means Subject accession
    case object sacc      extends OutputField[String]         { val label = toString }
    // means Subject accession.version
    case object saccver   extends OutputField[String]         { val label = toString }
    // means All subject accessions
    case object sallacc   extends OutputField[String]         { val label = toString }
    // means Subject sequence length
    case object slen      extends OutputField[Int]            { val label = toString }
    // means Start of alignment in query
    case object qstart    extends OutputField[Int]            { val label = toString }
    // means End of alignment in query
    case object qend      extends OutputField[Int]            { val label = toString }
    // means Start of alignment in subject
    case object sstart    extends OutputField[Int]            { val label = toString }
    // means End of alignment in subject
    case object send      extends OutputField[Int]            { val label = toString }
    // means Aligned part of query sequence
    case object qseq      extends OutputField[String]         { val label = toString }
    // means Aligned part of subject sequence
    case object sseq      extends OutputField[String]         { val label = toString }
    // means Expect value
    case object evalue    extends OutputField[Double]         { val label = toString }
    // means Bit score
    case object bitscore  extends OutputField[Long]           { val label = toString }
    // means Raw score
    case object score     extends OutputField[Long]           { val label = toString }
    // means Alignment length
    case object length    extends OutputField[Int]            { val label = toString }
    // means Percentage of identical matches
    case object pident    extends OutputField[Double]         { val label = toString }
    // case object nident extends OutputField[String]  { // means Number of identical matches
    // }
    // means Number of mismatches
    case object mismatch  extends OutputField[Int]      { val label = toString }
    // means Number of positive-scoring matches
    case object positive  extends OutputField[Int]      { val label = toString }
    // means Number of gap openings
    case object gapopen   extends OutputField[Int]      { val label = toString }
    // means Total number of gaps
    case object gaps      extends OutputField[Int]      { val label = toString }
    // case object ppos extends OutputField[String]  { // means Percentage of positive-scoring matches
    // }
    // case object frames extends OutputField[String]  { // means Query and subject frames separated by a '/'
    // }
    // means Query frame
    case object qframe      extends OutputField[String]   { val label = toString }
    // means Subject frame
    case object sframe      extends OutputField[String]   { val label = toString }
    // case object btop extends OutputField[String]  { // means Blast traceback operations (BTOP)
    // }
    // case object staxids extends OutputField[String]  { // means unique Subject Taxonomy ID(s), separated by a ';' (in numerical order)
    // }
    // case object sscinames extends OutputField[String]  { // means unique Subject Scientific Name(s), separated by a ';'
    // }
    // case object scomnames extends OutputField[String]  { // means unique Subject Common Name(s), separated by a ';'
    // }
    // case object sblastnames extends OutputField[String]  { // means unique Subject Blast Name(s), separated by a ';' (in alphabetical order)
    // }
    // case object sskingdoms extends OutputField[String]  { // means unique Subject Super Kingdom(s), separated by a ';' (in alphabetical order)
    // }
    // case object stitle extends OutputField[String]  { // means Subject Title
    // }
    // case object salltitles extends OutputField[String]  { // means All Subject Title(s), separated by a '<>'
    // }
    // case object sstrand extends OutputField[String]  { // means Subject Strand
    // }
    // case object qcovs extends OutputField[String]  { // means Query Coverage Per Subject
    // }
    // case object qcovhsp extends OutputField[String]  { // means Query Coverage Per HSP
    // }
  }

  import out._
  val defaultOutputFormat = OutputRecordFormat(
    qseqid :~: sseqid :~: pident :~: length :~: mismatch :~: gapopen :~: qstart :~: qend :~: sstart :~: send :~: out.evalue :~: bitscore :~: ∅
  )

  case class blastCmd(val cmd: AnyBlastCommand, val opts: List[AnyBlastOption]) {

    def toSeq: Seq[String] = Seq(cmd.name) ++ (opts flatMap { _.toSeq } )
  }
}

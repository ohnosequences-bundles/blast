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
  import ohnosequences.cosas.ops.typeSets.{CheckForAll, ToList }

  sealed trait AnyBlastCommand {

    val name: String

    type Arguments <: AnyTypeSet.Of[AnyBlastOption]
  }
  // TODO check that all args are options for this command
  abstract class BlastCommand(val name: String) extends AnyBlastCommand

  trait OutputFieldFor[C <: AnyBlastCommand] extends TypePredicate[AnyOutputField] {

    type Condition[Flds <: AnyOutputField] = C isIn Flds#Commands
  }

  implicit def getBlastCommandOps[BC <: AnyBlastCommand](cmd: BC): BlastCommandOps[BC] =
    BlastCommandOps(cmd)

  case class BlastCommandOps[Cmd <: AnyBlastCommand](val cmd: Cmd) {

    def withOptions[
      Opts <: AnyTypeSet.Of[AnyBlastOption]
    ](opts: Opts)(implicit
      ev: CheckForAll[Opts, OptionFor[Cmd]],
      toListEv: ToListOf[Opts, AnyBlastOption],
      allArgs: Cmd#Arguments ⊂ Opts
    ): BlastExec[Cmd,Opts] = BlastExec[Cmd, Opts](cmd: Cmd, opts: Opts)
  }

  sealed trait AnyBlastOption {

    type Commands <: AnyTypeSet.Of[AnyBlastCommand]
    val commands: Commands

    def toSeq: Seq[String]
  }
  abstract class BlastOption[Cmmnds <: AnyTypeSet.Of[AnyBlastCommand]](
    val commands: Cmmnds,
    val toSeq: Seq[String]
  )
  extends AnyBlastOption { type Commands = Cmmnds }

  trait OptionFor[C <: AnyBlastCommand] extends TypePredicate[AnyBlastOption] {

    type Condition[O <: AnyBlastOption] = C isIn O#Commands
  }

  case class BlastExec[
    Cmd <: AnyBlastCommand,
    Opts <: AnyTypeSet.Of[AnyBlastOption]// with AnyTypeSet.SupersetOf[Cmd#Arguments]
  ](
    val command: Cmd,
    val options: Opts
  )(implicit
    val ev: CheckForAll[Opts, OptionFor[Cmd]],
    val toListEv: ToListOf[Opts, AnyBlastOption],
    val allArgs: Cmd#Arguments ⊂ Opts
  ) {

    // TODO missing part about output format
    def toSeq: Seq[String] =  Seq(command.name) ++
                              ( (options.toListOf[AnyBlastOption]) flatMap { _.toSeq } )
  }




























  type blastn   = blastn.type
  case object blastn extends BlastCommand("blastn") {

    type Arguments = db :~: query :~: out :~: ∅
  }

  type blastp   = blastp.type
  case object blastp extends BlastCommand("blastp") {

    type Arguments = db :~: query :~: out :~: ∅
  }

  type blastx   = blastx.type
  case object blastx extends BlastCommand("blastx") {

    type Arguments = db :~: query :~: out :~: ∅
  }

  type tblastn   = tblastn.type
  case object tblastn extends BlastCommand("tblastn") {

    type Arguments = db :~: query :~: out :~: ∅
  }

  type tblastx   = tblastx.type
  case object tblastx extends BlastCommand("tblastx") {

    type Arguments = db :~: query :~: out :~: ∅
  }

  type makeblastdb   = makeblastdb.type
  case object makeblastdb extends BlastCommand("makeblastdb") {

    type Arguments = in :~: inputType :~: dbtype :~: ∅
  }

  type AllBlasts  = blastn :~: blastp :~: blastx :~: tblastn :~: tblastx :~: ∅
  val allBlasts   = blastn :~: blastp :~: blastx :~: tblastn :~: tblastx :~: ∅

  case class db(val file: File) extends BlastOption(
    allBlasts,
    Seq("-db", file.getCanonicalPath().toString)
  )

  case class query(val file: File) extends BlastOption(
    allBlasts,
    Seq("-query", file.getCanonicalPath().toString)
  )

  case class out(val file: File) extends BlastOption(
    allBlasts,
    Seq("-out", file.getCanonicalPath().toString)
  )



  case class numThreads(val number: Int)    extends BlastOption(
    allBlasts,
    Seq("-num_threads", s"${number}")
  )

  case class evalue(val number: Double)     extends BlastOption(
    allBlasts,
    Seq("-evalue", number.toString)
  )
  case class maxTargetSeqs(val number: Int) extends BlastOption(
    allBlasts,
    Seq("-max_target_seqs", s"${number}")
  )
  case object showGis                       extends BlastOption(
    allBlasts,
    Seq("-show_gis")
  )
  sealed abstract class BlastTask[BC <: BlastCommand](bc: BC, taskName: String) extends BlastOption(
    bc :~: ∅,
    Seq("-task", taskName)
  )

  case object task {

    // TODO finish writing all tasks here
    case object megablast       extends BlastTask( blastAPI.blastn, "megablast" )
    case object dcMegablast     extends BlastTask( blastAPI.blastn, "dc-megablast" )
    case object blastn          extends BlastTask( blastAPI.blastn, "blastn" )
    case object blastnShort     extends BlastTask( blastAPI.blastn, "blastn-short" )
    case object rmblastn        extends BlastTask( blastAPI.blastn, "rmblastn" )
    case object blastp          extends BlastTask( blastAPI.blastp, "blastp" )
    case object blastpFast      extends BlastTask( blastAPI.blastp, "blastp-fast" )
    case object blastpShort     extends BlastTask( blastAPI.blastp, "blastp-short" )
    case object blastx          extends BlastTask( blastAPI.blastx, "blastx" )
    case object blastxFast      extends BlastTask( blastAPI.blastx, "blastx-fast" )
    case object tblastn         extends BlastTask( blastAPI.tblastn, "tblastn" )
    case object tblastnFast     extends BlastTask( blastAPI.tblastn, "tblastn-fast" )
  }

  sealed trait BlastDBInputType
  object dbInputType {

    case object asn1_bin  extends BlastDBInputType
    case object asn1_txt  extends BlastDBInputType
    case object blastdb   extends BlastDBInputType
    case object fasta     extends BlastDBInputType
  }

  case class inputType(val tpe: BlastDBInputType) extends BlastOption(
    makeblastdb :~: ∅,
    Seq("-input_type", tpe.toString)
  )

  sealed trait BlastDBType
  object dbType {
    case object nucl extends BlastDBType
    case object prot extends BlastDBType
  }

  case class dbtype(val tpe: BlastDBType)   extends BlastOption(
    makeblastdb :~: ∅,
    Seq("-dbtype", tpe.toString)
  )
  case class title(val name: String)        extends BlastOption(makeblastdb :~: ∅, Seq("-title", name))

  case class in(val file: File) extends BlastOption(
    makeblastdb :~: ∅,
    Seq("-in", file.getCanonicalPath().toString)
  )

  sealed trait AnyOutputFormat extends AnyBlastOption {

    // TODO check it
    type Commands = AllBlasts
    val commands = allBlasts

    type OutputFormatType <: AnyOutputFormatType
    val outputFormatType: OutputFormatType

    type OutputRecordFormat <: AnyTypeSet.Of[AnyOutputField]
    val outputRecordFormat: OutputRecordFormat

    implicit val outputRecordFormatList: ToListOf[OutputRecordFormat, AnyOutputField]

    lazy val outputRecordFormatStr = (outputRecordFormat.toListOf[AnyOutputField] map {_.toString}).mkString(" ")
    lazy val code: Int = outputFormatType.code
    lazy val toSeq: Seq[String] = Seq("-outfmt", s"'${code} ${outputRecordFormatStr}'")
  }
  case class outfmt[
    T <: AnyOutputFormatType,
    OF <: AnyTypeSet.Of[AnyOutputField]
  ]
  (val outputFormatType: T, val outputRecordFormat: OF)(implicit
    val outputRecordFormatList: ToListOf[OF, AnyOutputField]
  )
    extends AnyOutputFormat
  {
    type OutputFormatType = T
    type OutputRecordFormat = OF
  }

  sealed trait AnyOutputFormatType { val code: Int }
  abstract class OutputFormatType(val code: Int) extends AnyOutputFormatType
  case object format {

    case object pairwise                  extends OutputFormatType(0)
    case object queryAnchoredShowIds      extends OutputFormatType(1)
    case object queryAnchoredNoIds        extends OutputFormatType(2)
    case object flatQueryAnchoredShowIds  extends OutputFormatType(3)
    case object flatQueryAnchoredNoIds    extends OutputFormatType(4)
    case object XML                       extends OutputFormatType(5)
    case object TSV                       extends OutputFormatType(6)
    case object TSVWithComments           extends OutputFormatType(7)
    case object TextASN1                  extends OutputFormatType(8)
    case object BinaryASN1                extends OutputFormatType(9)
    case object CSV                       extends OutputFormatType(10)
    case object BLASTArchiveASN1          extends OutputFormatType(11)
    case object JSONSeqalign              extends OutputFormatType(12)
  }

  sealed trait AnyOutputField extends AnyProperty {

    type Commands <: AnyTypeSet.Of[AnyBlastCommand]
  }

  trait OutputField[V] extends AnyOutputField {

    type Raw = V
    lazy val label: String = toString
  }

  case object out {

    trait ForCommands[Cmmnds <: AnyTypeSet.Of[AnyBlastCommand]] extends AnyOutputField {

      type Commands = Cmmnds
    }

    // means Query Seq-id
    case object qseqid    extends OutputField[String] with ForCommands[AllBlasts]
    // means Query GI
    case object qgi       extends OutputField[String] with ForCommands[AllBlasts]
    // means Query accesion
    case object qacc      extends OutputField[String] with ForCommands[AllBlasts]
    // means Query accesion.version
    case object qaccver   extends OutputField[Int] with ForCommands[AllBlasts]
    // means Query sequence length
    case object qlen      extends OutputField[Int] with ForCommands[AllBlasts]
    // means Subject Seq-id
    case object sseqid    extends OutputField[String] with ForCommands[AllBlasts]
    // means All subject Seq-id(s), separated by a ';'
    case object sallseqid extends OutputField[List[String]] with ForCommands[AllBlasts]
    // means Subject GI
    case object sgi       extends OutputField[String] with ForCommands[AllBlasts]
    // means All subject GIs
    case object sallgi    extends OutputField[List[String]] with ForCommands[AllBlasts]
    // means Subject accession
    case object sacc      extends OutputField[String] with ForCommands[AllBlasts]
    // means Subject accession.version
    case object saccver   extends OutputField[String] with ForCommands[AllBlasts]
    // means All subject accessions
    case object sallacc   extends OutputField[String] with ForCommands[AllBlasts]
    // means Subject sequence length
    case object slen      extends OutputField[Int] with ForCommands[AllBlasts]
    // means Start of alignment in query
    case object qstart    extends OutputField[Int] with ForCommands[AllBlasts]
    // means End of alignment in query
    case object qend      extends OutputField[Int] with ForCommands[AllBlasts]
    // means Start of alignment in subject
    case object sstart    extends OutputField[Int] with ForCommands[AllBlasts]
    // means End of alignment in subject
    case object send      extends OutputField[Int] with ForCommands[AllBlasts]
    // means Aligned part of query sequence
    case object qseq      extends OutputField[String] with ForCommands[AllBlasts]
    // means Aligned part of subject sequence
    case object sseq      extends OutputField[String] with ForCommands[AllBlasts]
    // means Expect value
    case object evalue    extends OutputField[Double] with ForCommands[AllBlasts]
    // means Bit score
    case object bitscore  extends OutputField[Long] with ForCommands[AllBlasts]
    // means Raw score
    case object score     extends OutputField[Long] with ForCommands[AllBlasts]
    // means Alignment length
    case object length    extends OutputField[Int] with ForCommands[AllBlasts]
    // means Percentage of identical matches
    case object pident    extends OutputField[Double] with ForCommands[AllBlasts]
    // case object nident extends OutputField[String]  { // means Number of identical matches
    // }
    // means Number of mismatches
    case object mismatch  extends OutputField[Int] with ForCommands[AllBlasts]
    // means Number of positive-scoring matches
    case object positive  extends OutputField[Int] with ForCommands[AllBlasts]
    // means Number of gap openings
    case object gapopen   extends OutputField[Int] with ForCommands[AllBlasts]
    // means Total number of gaps
    case object gaps      extends OutputField[Int] with ForCommands[AllBlasts]
    // case object ppos extends OutputField[String]  { // means Percentage of positive-scoring matches
    // }
    // case object frames extends OutputField[String]  { // means Query and subject frames separated by a '/'
    // }
    // means Query frame
    case object qframe      extends OutputField[String] with ForCommands[AllBlasts]
    // means Subject frame
    case object sframe      extends OutputField[String] with ForCommands[AllBlasts]
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

  val defaultOutputFields = qseqid      :~:
                            sseqid      :~:
                            pident      :~:
                            length      :~:
                            mismatch    :~:
                            gapopen     :~:
                            qstart      :~:
                            qend        :~:
                            sstart      :~:
                            send        :~:
                            out.evalue  :~:
                            bitscore    :~: ∅


}

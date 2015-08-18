package ohnosequencesBundles.statika

object blastAPI {

  import ohnosequences.cosas._, typeSets._, properties._, types._, records._
  import ohnosequences.cosas.ops.typeSets.{ CheckForAll, ToList }

  import java.io.File

  /*

    ## BLAST commands

    This type represents a command part of the BLAST suite (`blastn`, `makeblastdb`, etc). The instances are below.
  */
  sealed trait AnyBlastCommand {

    val name: String

    // TODO check that all args are options for this command. Will need an itermediate type
    type Arguments <: AnyRecord { type Fields <: AnyFields.withBound[AnyBlastOption] }
  }
  abstract class BlastCommand(val name: String) extends AnyBlastCommand

  /*
    ## BLAST options

    A BLAST option. The same option is valid for different BLAST commands, thus the `Commands` typeset. `toSeq` builds a `Seq[String]` representation of the option.
  */
  sealed trait AnyBlastOption extends AnyProperty {

    type Commands <: AnyTypeSet.Of[AnyBlastCommand]

    val valueToString: Raw => String
  }

  // this is most likely crap
  import shapeless._, poly._
  object optionValueToSeq extends shapeless.Poly1 {

    implicit def default[BO <: AnyBlastOption](implicit option: BO, conv: ValueOf[BO] => String) =
      at[ValueOf[BO]]{ v: ValueOf[BO] => Seq( option.label, conv(v) ) }
  }

  trait ForCommands[Cmmnds <: AnyTypeSet.Of[AnyBlastCommand]]
  extends AnyBlastOption { type Commands = Cmmnds }

  abstract class BlastOption[V](val valueToString: V => String) extends AnyBlastOption {

    type Raw = V
    lazy val label: String = s"-${toString}"
  }

  /*
    This is a type predicate over BLAST options. An instance of `Condition[O]` tells you that `O` is a valid option for `C`.
  */
  trait OptionFor[C <: AnyBlastCommand] extends TypePredicate[AnyBlastOption] {

    type Condition[O <: AnyBlastOption] = C isIn O#Commands
  }

  /*
    ## BLAST statements

    Something valid that you can execute using BLAST.
  */
  // case class BlastStatement[
  //   Cmd <: AnyBlastCommand,
  //   Opts <: AnyTypeSet.Of[AnyBlastOption]
  // ](
  //   val command: Cmd,
  //   val options: Opts
  // )(implicit
  //   val ev: CheckForAll[Opts, OptionFor[Cmd]],
  //   val toListEv: ToListOf[Opts, AnyBlastOption],
  //   val allArgs: Cmd#Arguments ⊂ Opts
  // )
  // {
  //   def toSeq: Seq[String] =  Seq(command.name) ++
  //                             ( (options.toListOf[AnyBlastOption]) flatMap { _.toSeq } )
  // }
  //
  // implicit def getBlastCommandOps[BC <: AnyBlastCommand](cmd: BC): BlastCommandOps[BC] =
  //   BlastCommandOps(cmd)
  //
  // case class BlastCommandOps[Cmd <: AnyBlastCommand](val cmd: Cmd) {
  //
  //   def withOptions[
  //     Opts <: AnyTypeSet.Of[AnyBlastOption]
  //   ](opts: Opts)(implicit
  //     ev: CheckForAll[Opts, OptionFor[Cmd]],
  //     toListEv: ToListOf[Opts, AnyBlastOption],
  //     allArgs: Cmd#Arguments ⊂ Opts
  //   ): BlastStatement[Cmd,Opts] = BlastStatement(cmd, opts)
  // }

  /*
    ## BLAST commands
  */
  type blastn   = blastn.type
  case object blastn extends BlastCommand("blastn") {
    case object arguments extends Record(db :&: query :&: out :&: □)
    type Arguments = arguments.type
  }

  type blastp   = blastp.type
  case object blastp extends BlastCommand("blastp") {
    case object arguments extends Record(db :&: query :&: out :&: □)
    type Arguments = arguments.type
  }

  type blastx   = blastx.type
  case object blastx extends BlastCommand("blastx") {
    case object arguments extends Record(db :&: query :&: out :&: □)
    type Arguments = arguments.type
  }

  type tblastn   = tblastn.type
  case object tblastn extends BlastCommand("tblastn") {
    case object arguments extends Record(db :&: query :&: out :&: □)
    type Arguments = arguments.type
  }

  type tblastx   = tblastx.type
  case object tblastx extends BlastCommand("tblastx") {
    case object arguments extends Record(db :&: query :&: out :&: □)
    type Arguments = arguments.type
  }

  type makeblastdb   = makeblastdb.type
  case object makeblastdb extends BlastCommand("makeblastdb") {

    case object arguments extends Record(in :&: input_type :&: dbtype :&: □)
    type Arguments = arguments.type
  }

  /* Just a convenience type for all the blast-like commands (search query in db) */
  type AllBlasts  = blastn :~: blastp :~: blastx :~: tblastn :~: tblastx :~: ∅
  val allBlasts   = blastn :~: blastp :~: blastx :~: tblastn :~: tblastx :~: ∅


  /*
    ## BLAST options

    All the possible options for all BLAST commands. Quite comprehensive I think.
  */
  case object db    extends BlastOption[File](f => f.getCanonicalPath.toString) with ForCommands[AllBlasts]
  case object query extends BlastOption[File](f => f.getCanonicalPath.toString) with ForCommands[AllBlasts]
  case object out   extends BlastOption[File](f => f.getCanonicalPath.toString) with ForCommands[AllBlasts]

  case object num_threads     extends BlastOption[Int](n => n.toString)     with ForCommands[AllBlasts]
  case object evalue          extends BlastOption[Double](n => n.toString)  with ForCommands[AllBlasts]
  case object max_target_seqs extends BlastOption[Int](n => n.toString)     with ForCommands[AllBlasts]
  case object show_gis        extends BlastOption[Boolean](t => "")         with ForCommands[AllBlasts]

  /*
    #### BLAST tasks

    These are styles for running BLAST. You specify them like `task.blastpFast`.
  */
  sealed abstract class BlastTask[BC <: AnyBlastCommand](val bc: BC, val taskName: String)

  case class task[BC <: AnyBlastCommand](val cmd: BC)
  extends BlastOption[BlastTask[BC]](bt => bt.taskName) with ForCommands[BC :~: ∅]

  case object task {

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















  /*
    ### BLAST output

    There is a lot that can be specified for BLAST output. See below for output format types and output fields.
  */
  // sealed trait AnyOutputFormat extends AnyBlastOption {
  //
  //   // TODO check it
  //   type Commands = AllBlasts
  //   val commands = allBlasts
  //
  //   type OutputFormatType <: AnyOutputFormatType
  //   val outputFormatType: OutputFormatType
  //
  //   type OutputRecordFormat <: AnyTypeSet.Of[AnyOutputField]
  //   val outputRecordFormat: OutputRecordFormat
  //
  //   implicit val outputRecordFormatList: ToListOf[OutputRecordFormat, AnyOutputField]
  //
  //   lazy val outputRecordFormatStr = (outputRecordFormat.toListOf[AnyOutputField] map {_.toString}).mkString(" ")
  //   lazy val code: Int = outputFormatType.code
  //   lazy val toSeq: Seq[String] = Seq("-outfmt", s"'${code} ${outputRecordFormatStr}'")
  // }
  // case class outfmt[
  //   T <: AnyOutputFormatType,
  //   OF <: AnyTypeSet.Of[AnyOutputField]
  // ]
  // (
  //   val outputFormatType: T,
  //   val outputRecordFormat: OF
  // )(implicit
  //   val outputRecordFormatList: ToListOf[OF, AnyOutputField]
  // )
  // extends AnyOutputFormat {
  //
  //   type OutputFormatType = T
  //   type OutputRecordFormat = OF
  // }















  /*
    #### `makeblastdb`-specific options
  */
  case object title extends BlastOption[String](x => x) with ForCommands[makeblastdb.type :~: ∅]
  case object in extends BlastOption[File](f => f.getCanonicalPath.toString) with ForCommands[makeblastdb.type :~: ∅]

  case object input_type extends BlastOption[BlastDBInputType](t => t.toString) with ForCommands[makeblastdb.type :~: ∅]
  sealed trait BlastDBInputType
  object dbInputType {

    case object asn1_bin  extends BlastDBInputType
    case object asn1_txt  extends BlastDBInputType
    case object blastdb   extends BlastDBInputType
    case object fasta     extends BlastDBInputType
  }

  case object dbtype extends BlastOption[BlastDBType](t => t.toString) with ForCommands[makeblastdb.type :~: ∅]
  sealed trait BlastDBType
  object BlastDBType {
    case object nucl extends BlastDBType
    case object prot extends BlastDBType
  }

























  /*
    ### BLAST output formats and fields

    A lot of different outputs, plus the possibility of choosing fields for CSV/TSV output.
  */
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

  trait OutputFieldFor[C <: AnyBlastCommand] extends TypePredicate[AnyOutputField] {

    type Condition[Flds <: AnyOutputField] = C isIn Flds#Commands
  }

  /* Inside this object you have all the possible fields that you can specify as output */
  case object outFields {

    /* Auxiliary type for setting the valid commands for an output field. */
    trait ForCommands[Cmmnds <: AnyTypeSet.Of[AnyBlastCommand]] extends AnyOutputField {

      type Commands = Cmmnds
    }

    /* Query Seq-id */
    case object qseqid    extends OutputField[String] with ForCommands[AllBlasts]
    /* Query GI */
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
    // means Number of identical matches
    // case object nident extends OutputField[String]  {
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

  import outFields._

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
                            outFields.evalue  :~:
                            bitscore    :~: ∅
}

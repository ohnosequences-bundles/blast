package ohnosequencesBundles.statika

case object blastAPI {

  import ohnosequences.cosas._, typeSets._, properties._, types._, records._
  import ohnosequences.cosas.ops.typeSets.{ CheckForAll, ToList }
  import java.io.File

  sealed trait AnyBlastCommand extends AnyProperty {

    lazy val label: String = toString

    // TODO check that all args are options for this command. Will need an itermediate type
    type Arguments  <: AnyRecord { type Fields <: AnyFields.withBound[AnyBlastOption] }
    type Options    <: AnyRecord { type Fields <: AnyFields.withBound[AnyBlastOption] }

    val defaults: ValueOf[Options]
    // TODO much fields so record
    type Raw = ValueOf[Arguments]
  }

  sealed trait AnyBlastOption extends AnyProperty {

    // type Commands <: AnyTypeSet.Of[AnyBlastCommand]
    val valueToString: Raw => String

    lazy val label: String = s"-${toString}"
  }
  abstract class BlastOption[V](val valueToString: V => String) extends AnyBlastOption { type Raw = V }

  // this is most likely crap
  import shapeless._, poly._
  object optionValueToSeq extends shapeless.Poly1 {

    implicit def default[BO <: AnyBlastOption](implicit option: BO, conv: ValueOf[BO] => String) =
      at[ValueOf[BO]]{ v: ValueOf[BO] => Seq( option.label, conv(v) ) }
  }

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

  type blastn   = blastn.type
  case object blastn extends AnyBlastCommand {

    case object arguments extends Record(db :&: query :&: out :&: □)
    type Arguments = arguments.type
    case object options extends Record(num_threads :&: task(blastn) :&: □)
    type Options = options.type

    val defaults = options :=
      num_threads(1)            :~: 
      task(blastn)(task.blastn) :~: ∅
  }

  type blastp   = blastp.type
  case object blastp extends AnyBlastCommand {

    case object arguments extends Record(db :&: query :&: out :&: □)
    type Arguments = arguments.type
    case object options extends Record(num_threads :&: □)
    type Options = options.type

    val defaults = options := num_threads(1) :~: ∅
  }

  type blastx   = blastx.type
  case object blastx extends AnyBlastCommand {

    case object arguments extends Record(db :&: query :&: out :&: □)
    type Arguments = arguments.type
    case object options extends Record(num_threads :&: □)
    type Options = options.type

    val defaults = options := num_threads(1) :~: ∅
  }

  type tblastn   = tblastn.type
  case object tblastn extends AnyBlastCommand {

    case object arguments extends Record(db :&: query :&: out :&: □)
    type Arguments = arguments.type
    case object options extends Record(num_threads :&: □)
    type Options = options.type

    val defaults = options := num_threads(1) :~: ∅
  }

  type tblastx   = tblastx.type
  case object tblastx extends AnyBlastCommand {

    case object arguments extends Record(db :&: query :&: out :&: □)
    type Arguments = arguments.type
    case object options extends Record(num_threads :&: □)
    type Options = options.type

    val defaults = options := num_threads(1) :~: ∅
  }

  type makeblastdb   = makeblastdb.type
  case object makeblastdb extends AnyBlastCommand {

    case object arguments extends Record(in :&: input_type :&: dbtype :&: □)
    type Arguments = arguments.type
    case object options extends Record(title :&: □)
    type Options = options.type

    val defaults = options := title("") :~: ∅
  }

  case object db    extends BlastOption[File](f => f.getCanonicalPath.toString)
  case object query extends BlastOption[File](f => f.getCanonicalPath.toString)
  case object out   extends BlastOption[File](f => f.getCanonicalPath.toString)

  case object num_threads     extends BlastOption[Int](n => n.toString)
  case object evalue          extends BlastOption[Double](n => n.toString)
  case object max_target_seqs extends BlastOption[Int](n => n.toString)
  case object show_gis        extends BlastOption[Boolean](t => "")

  case class task[BC <: AnyBlastCommand](val cmd: BC) extends BlastOption[BlastTask[BC]](bt => bt.taskName)

  sealed abstract class BlastTask[BC <: AnyBlastCommand](val bc: BC, val taskName: String)
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
    #### `makeblastdb`-specific options
  */
  case object title extends BlastOption[String](x => x)
  case object in extends BlastOption[File](f => f.getCanonicalPath.toString)

  case object input_type extends BlastOption[BlastDBInputType](t => t.toString)
  sealed trait BlastDBInputType
  object dbInputType {

    case object asn1_bin  extends BlastDBInputType
    case object asn1_txt  extends BlastDBInputType
    case object blastdb   extends BlastDBInputType
    case object fasta     extends BlastDBInputType
  }

  case object dbtype extends BlastOption[BlastDBType](t => t.toString)
  sealed trait BlastDBType
  case object BlastDBType {
    case object nucl extends BlastDBType
    case object prot extends BlastDBType
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
    case object qseqid    extends OutputField[String]
    /* Query GI */
    case object qgi       extends OutputField[String]
    // means Query accesion
    case object qacc      extends OutputField[String]
    // means Query accesion.version
    case object qaccver   extends OutputField[Int]
    // means Query sequence length
    case object qlen      extends OutputField[Int]
    // means Subject Seq-id
    case object sseqid    extends OutputField[String]
    // means All subject Seq-id(s), separated by a ';'
    case object sallseqid extends OutputField[List[String]]
    // means Subject GI
    case object sgi       extends OutputField[String]
    // means All subject GIs
    case object sallgi    extends OutputField[List[String]]
    // means Subject accession
    case object sacc      extends OutputField[String]
    // means Subject accession.version
    case object saccver   extends OutputField[String]
    // means All subject accessions
    case object sallacc   extends OutputField[String]
    // means Subject sequence length
    case object slen      extends OutputField[Int]
    // means Start of alignment in query
    case object qstart    extends OutputField[Int]
    // means End of alignment in query
    case object qend      extends OutputField[Int]
    // means Start of alignment in subject
    case object sstart    extends OutputField[Int]
    // means End of alignment in subject
    case object send      extends OutputField[Int]
    // means Aligned part of query sequence
    case object qseq      extends OutputField[String]
    // means Aligned part of subject sequence
    case object sseq      extends OutputField[String]
    // means Expect value
    case object evalue    extends OutputField[Double]
    // means Bit score
    case object bitscore  extends OutputField[Long]
    // means Raw score
    case object score     extends OutputField[Long]
    // means Alignment length
    case object length    extends OutputField[Int]
    // means Percentage of identical matches
    case object pident    extends OutputField[Double]
    // means Number of identical matches
    // case object nident extends OutputField[String]  {
    // }
    // means Number of mismatches
    case object mismatch  extends OutputField[Int]
    // means Number of positive-scoring matches
    case object positive  extends OutputField[Int]
    // means Number of gap openings
    case object gapopen   extends OutputField[Int]
    // means Total number of gaps
    case object gaps      extends OutputField[Int]
    // case object ppos extends OutputField[String]  { // means Percentage of positive-scoring matches
    // }
    // case object frames extends OutputField[String]  { // means Query and subject frames separated by a '/'
    // }
    // means Query frame
    case object qframe      extends OutputField[String]
    // means Subject frame
    case object sframe      extends OutputField[String]
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

package ohnosequencesBundles.blast.test

import ohnosequencesBundles.statika.blastAPI._
import ohnosequences.cosas.typeSets._

import java.io.File

class apiTests extends org.scalatest.FunSuite {

  val dbFile    = new File("/tmp/buh")
  val queryFile = new File("/tmp/query")
  val outFile   = new File("/tmp/blastout")

  test("command generation") {

    val uh =
      blastn withOptions
        db(dbFile)        :~:
        query(queryFile)  :~:
        out(outFile)      :~:
        outfmt(format.TSV, defaultOutputFields) :~: ∅

    assert(
      uh.toSeq.mkString(" ") ===
      "blastn -db /tmp/buh -query /tmp/query -out /tmp/blastout -outfmt '6 qseqid sseqid pident length mismatch gapopen qstart qend sstart send evalue bitscore'"
    )
  }
}

package ohnosequencesBundles.statika

import ohnosequences.statika._, bundles._, instructions._
import java.io.File


abstract class Blast (version: String) extends Bundle() {

    val blastDistribution = "-x64-linux"
    val blastPath = s"ncbi-blast-${version}+"
    val blastBinPath = new File(s"${blastPath}/bin/").getAbsolutePath
    val usrbin = "/usr/bin/"

    val commands: Set[String] = Set(
      "blast_formatter",
      "blastdb_aliastool",
      "blastdbcheck",
      "blastdbcmd",
      "blastn",
      "blastp",
      "blastx",
      "convert2blastmask",
      "deltablast",
      "dustmasker",
      "makeblastdb",
      "makembindex",
      "makeprofiledb",
      "psiblast",
      "rpsblast",
      "rpstblastn",
      "segmasker",
      "tblastn",
      "tblastx",
      "windowmasker"
      )


    def linkCommand(cmd: String): Results =
        Seq("ln", "-s", s"${blastBinPath}", s"${usrbin}/${cmd}")


    def install: Results = {

        Seq("aws", "s3", "cp", s"s3://resources.ohnosequences.com/blast/${version}/${blastPath}${blastDistribution}.tar.gz", "./") -&- Seq("tar","-xvf", s"${blastPath}${blastDistribution}.tar.gz") ->-
        commands.foldLeft[Results](
            Seq("echo", "linking blast binaries")
          ){ (acc, cmd) => acc ->- linkCommand(cmd) } ->-
          success(s"${bundleName} is installed")
    }

}

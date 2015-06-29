package ohnosequencesBundles.statika

import ohnosequences.statika._, bundles._, instructions._




case object blast {


  case object Blast extends Bundle() {

    def install: Results = {

      import ammonite.ops._
      val wd = cwd

      val getFiles =
        Seq("aws", "s3", "cp", "s3://resources.ohnosequences.com/blast/ncbi-blast-2.2.30+-x64-linux.tar.gz", "./") -&- Seq("tar","-xvf", "ncbi-blast-2.2.30+-x64-linux.tar.gz")

      val blastBinPath = wd/"ncbi-blast-2.2.30+"/"bin"
      val usrbin = root/"usr"/"bin"

      ls! blastBinPath | { x => ln.s(blastBinPath/x.last, usrbin/x.last) }

      if ( exists(usrbin/"blastn") )
        success(bundleFullName + " is installed")
      else
        failure("Something went wrong with the linking :(")

    }

  }

}

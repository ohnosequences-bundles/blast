package ohnosequencesBundles.statika

import ohnosequences.statika._, bundles._, instructions._
import ohnosequences.awstools.regions.Region._
import ohnosequences.cosas.typeSets._


case object blast {

	case object ami extends amzn_ami_pv_64bit(Ireland)(1)

  case object Blast extends Bundle() {

    def install: Results = {

      import ammonite.ops._
      val wd = cwd

      val getFiles =
        Seq("aws", "s3", "cp", "s3://resources.ohnosequences.com/blast/ncbi-blast-2.2.30+-x64-linux.tar.gz", "./") -&- Seq("tar","-xvf", "ncbi-blast-2.2.30+-x64-linux.tar.gz")

      val makeblastdbBin = "makeblastdbBin" 
      val blastBinPath = "SPAdes-3.1.0-Linux"/"bin"/"*"
      val usrbin = root/"usr"/"bin"

      ln.s(wd/blastBinPath, usrbin/)

      //Seq("ln", "-s","./SPAdes-3.1.0-Linux/bin/spades.py","/usr/bin/")

      if ( exists(usrbin/makeblastdbBin) )
        success(fullName + " is installed")
      else
        failure("Something went wrong with the linking :(")

    }

  }

}

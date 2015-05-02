package ohnosequences-bundles.statika

import ohnosequences.statika._, bundles._, instructions._


case object blast {

  case object Blast extends Bundle() {

    def install: Results = {
      // do someting here
      success(fullName + " is installed")
    }
  }

}

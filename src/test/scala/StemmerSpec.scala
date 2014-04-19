package test

import org.specs2.mutable._
import org.specs2.execute._
import net.theophil.relatedtexts.GermanStemmer

class StemmerSpec extends Specification {
  "GermanStemmer " should {
    "calculate correct stems for words in diffs.txt" in {
      // Read list from http://snowball.tartarus.org/algorithms/german/diffs.txt
      // and compare output of stemming algorithm to diffs.txt
      // diffs.txt contains two lines with line breaks, that is corrected in local copy
      val test = io.Source.fromInputStream(getClass.getResourceAsStream("/test_data/diffs.txt"), "UTF-8")
      Result.unit( for(line <- test.getLines()) {
        val iSplit = line.indexWhere(_.isSpaceChar)
        val strIn = line.substring(0, iSplit).trim
        val strOut = line.substring(iSplit).trim

        val strOutStemmer = GermanStemmer(strIn)
        strOut must beEqualTo(strOutStemmer)
      })
    }
  }
}
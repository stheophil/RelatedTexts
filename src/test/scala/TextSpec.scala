package test

import org.specs2.mutable._
import net.theophil.relatedtexts._

class TextSpec extends Specification {
  "TextStatistics " should {
    "count ngrams" in {
      val stats = TextStatistics(
        "Ein kleiner Text auf Deutsch mit mehreren Wörtern und Umlauten.",
        Seq("keyword1", "keyword2"),
        Set("ein", "auf", "mit", "und")
      )

      val words = Set("klein", "text", "deutsch", "mehr", "wort", "umlaut",
        "klein text", "text deutsch", "deutsch mehr", "mehr wort", "wort umlaut",
        "klein text deutsch", "text deutsch mehr", "deutsch mehr wort", "mehr wort umlaut")

      stats.ngramCount.keys must containTheSameElementsAs(words.toSeq)
      ((_:Int) must beEqualTo(1)).forall(stats.ngramCount.values)
    }
  }
}
package net.theophil.relatedtexts.samples

import net.theophil.relatedtexts._
import play.api.libs.json._

object TextMatcherExample {
  def main(args: Array[String]) {
    val scraped = new java.io.File("/Users/sebastian/Dropbox/Programming/relatedtexts_testdata/scraped")
    if(!scraped.exists()) {
      println("Folder with scraped texts does not exist")
      sys.exit(1)
    }

    val texts = scraped.listFiles().map( file => {
      DefaultText(
        file.getName,
        io.Source.fromFile(file, "UTF-8").getLines().mkString(" "),
        Seq.empty[String],
        ""
      )
    })

    val resultlist = new collection.mutable.ArrayBuffer[DefaultResult]()
    TextMatcher(
      samples.loadSampleTexts,
      texts,
      (m: (DefaultText, TextMatch[DefaultText])) => {
        resultlist += new DefaultResult(
          m._1,
          m._2.value,
          Seq(new DefaultResultMatch(
            m._2.matched,
            m._2.value,
            m._2.words.map(_._1)
          ))
        )
      },
      /*limit*/ 4.0
    )

    writeMatches(Json.toJson(
      resultlist.sortBy(_.confidence)(Ordering[Double].reverse)
    ).toString())
  }
}
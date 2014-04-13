package net.theophil.relatedtexts

import scala.collection.mutable
import java.io.{FileOutputStream, OutputStreamWriter}
import java.nio.charset.Charset
import java.util.Date

object TestStemmer {
  def main(args: Array[String]) {
    // Read list from http://snowball.tartarus.org/algorithms/german/diffs.txt
    // and compare output of stemming algorithm to diffs.txt
    // diffs.txt contains two lines with line breaks, that is corrected in local copy
    val test = io.Source.fromFile("test/diffs.txt", "UTF-8")
    for(line <- test.getLines()) {
      val iSplit = line.indexWhere(_.isSpaceChar)
      val strIn = line.substring(0, iSplit).trim
      val strOut = line.substring(iSplit).trim

      val strOutStemmer = GermanStemmer(strIn)
      Console.println("in: " + strIn + " expected: " + strOut + " out: " + strOutStemmer)
      assert(strOut == strOutStemmer)
    }
  }
}

import play.api.libs.json._ // JSON library
import play.api.libs.json.Reads._ // Custom validation helpers
import play.api.libs.functional.syntax._ // Combinator syntax

object JSON {
  // JSON format of www.wahlversprechen2013.de/json/author/Koalitionsvertrag
  case class InputStatement(id: Int, title: String, quote: String, tags: Seq[String])

  implicit val statementReads: Reads[InputStatement] = (
    (JsPath \ "id").read[Int] and
      (JsPath \ "title").read[String] and
      (JsPath \ "quote").read[String] and
      (JsPath \ "tags").read[Seq[String]]
    )(InputStatement.apply _)
}

object StatTest {
  import JSON._

  case class OutputStatement(id: Int, title: String, filtered_text: Seq[String], tags: Seq[String])
  implicit val outputStatementWrites: Writes[OutputStatement] = (
    (JsPath \ "id").write[Int] and
      (JsPath \ "title").write[String] and
      (JsPath \ "filtered_text").write[Seq[String]] and
      (JsPath \ "tags").write[Seq[String]]
    )(unlift(OutputStatement.unapply))

  def main(args: Array[String]) {
    val jsonString = io.Source.fromFile("test/koalitionsvertrag.json").getLines().mkString("\n")
    val json: JsValue = Json.parse(jsonString)
    val result: JsResult[Seq[InputStatement]] = json.validate[Seq[InputStatement]]

    val top1kWords = (for(line <- io.Source.fromFile("test/top1000de.txt", "UTF-16").getLines) yield
      GermanStemmer(line)
     ).toSet

    result match {
      case s: JsSuccess[Seq[InputStatement]] => {
        val inputWordSeq = s.get.map{ input =>
          val filtered_text = input.title + " " + input.quote
          val analytics = TextStatistics(filtered_text, Seq.empty[String], top1kWords)
          (input, analytics.ngramCount.toSeq.map(_._1.mkString))
        }

        val wordCount = inputWordSeq.map( _._2 ).flatten.
          foldLeft(Map.empty[String, Int])( (map, word) => {
            map + (word -> (map.getOrElse(word, 0) + 1))
        })

        val output = inputWordSeq.map{ case (input, wordSeq) => {
          val wordSeqFreq = wordSeq.
            map{ word => (word, wordCount(word)) }.
            sortBy(_._2).toSeq.map( t => t._1 + ":" + t._2)
          OutputStatement( input.id, input.title, wordSeqFreq, input.tags )
        }}

        val json = Json.toJson(output)
        
        val fos = new FileOutputStream("tmp/koalition_filtered.json")
        val file = new OutputStreamWriter(fos, Charset.forName("UTF-8"))
        file.write(json.toString())
        file.close()
        fos.close()
      }
      case e: JsError => println("Errors: " + JsError.toFlatJson(e).toString())
    }
  }
}

object FeedMatcherTest {
  import JSON._

  def removeMarkdownLink(text: String) : String = {
    val markdownLink = """\[([^\]]*)\]\(([^\)]*)\)""".r("text", "href")
    markdownLink.replaceAllIn( text, _ group "text" )
  }

  def main(args: Array[String]) {
    val cacheFile = "tmp/feedmatcher_cache"
    val jsonFile = "tmp/matches.json"

    val feeds = List("http://rss.sueddeutsche.de/rss/Politik",
      "http://www.welt.de/politik/deutschland/?service=Rss",
      "http://www.welt.de/wirtschaft/?service=Rss",
      "http://rss.sueddeutsche.de/rss/Wirtschaft",
      "http://newsfeed.zeit.de/politik/index",
      "http://newsfeed.zeit.de/wirtschaft/index",
      "http://newsfeed.zeit.de/gesellschaft/index",
      "http://www.faz.net/rss/aktuell/politik/",
      "http://www.faz.net/rss/aktuell/wirtschaft",
      "http://www.bundesregierung.de/SiteGlobals/Functions/RSSFeed/DE/RSSNewsfeed/RSS_Breg_artikel/RSSNewsfeed.xml?nn=392282"
    )

    val jsonString = io.Source.fromFile("test/koalitionsvertrag.json", "UTF-8").getLines().mkString("\n")
    val json: JsValue = Json.parse(jsonString)
    val seqInputs: JsResult[Seq[InputStatement]] = json.validate[Seq[InputStatement]]

    val statements = seqInputs.get.map( input => {
      Statement(
        removeMarkdownLink(input.title),
        removeMarkdownLink(input.title + " " + input.quote),
        input.tags,
        "http://www.wahlversprechen2013.de/item/" + input.id
      )
    })

    FeedMatcher(feeds, statements, 100, FeedMatcherCache.fromFile(cacheFile)) match {
      case (results, cache) => {
        val fos = new FileOutputStream(jsonFile)
        val fileJSON = new OutputStreamWriter(fos, Charset.forName("UTF-8"))
        
        val jsonMatches = Json.toJson(results)
        fileJSON.write("var matches = " + jsonMatches.toString())
        
        fileJSON.close()
        fos.close()

        cache.serialize(cacheFile)
      }
    }

  }

}
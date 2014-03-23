import scala.collection.mutable
import text._
import feed._

import java.io.{ObjectInputStream, ObjectOutputStream, FileInputStream, FileOutputStream, FileNotFoundException, FileWriter}
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
        val file = new FileWriter("tmp/koalition_filtered.json")
        file.write(json.toString())
        file.close()
      }
      case e: JsError => println("Errors: " + JsError.toFlatJson(e).toString())
    }
  }
}

object FeedMatcher {
  case class Statement(title: String, override val text: String, override val keywords: Seq[String], val url: String) extends Analyzable

  def initialize(seqInputs: Seq[JSON.InputStatement], stopwords: io.Source, outName: String) {
    val analyzer = new Analyzer(
      seqInputs.map( input =>
        Statement(
          input.title,
          input.title + " " + input.quote,
          input.tags,
          "http://www.wahlversprechen2013.de/item/" + input.id
        )
      ),
      stopwords
    )
    serialize(outName, analyzer)
  }

  type AnalyzableItem = feed.Item with Analyzable
  type MapItemUrlLength = collection.mutable.SynchronizedMap[String, Int] // url -> length
  type BestMatches = collection.mutable.SortedSet[(Statement, TextMatch[AnalyzableItem])]

  implicit val orderByMatchValue = Ordering.by[(Statement, TextMatch[AnalyzableItem]), Double](_._2.value)

  def update(feeds: List[String], analyzer: Analyzer[Statement], mapUrlToLength: MapItemUrlLength, matches: BestMatches) {
    import scala.concurrent._
    import scala.concurrent.ExecutionContext.Implicits.global

    val mapUrlToLengthLastRun = mapUrlToLength.clone() // cannot swap out mapUrlToLength
    mapUrlToLength.clear()

    var timeStart = new Date().getTime()
    val feeditems =
      Await.result(
        Future.sequence( feeds.map( url => {
          future {
            feed.Feed.newItemsWithText(
              url,
              mapUrlToLengthLastRun.get(_).getOrElse(0), // length of seen article or 0
              /* out */ mapUrlToLength
            ).map(
              item => new Item(item.title, item.link, item.text, item.faviconUrl) with Analyzable
            )
          }
        })),
        duration.Duration(5, duration.MINUTES)
      ).flatten

    var timeEnd = new Date().getTime()
    Console.println("[Info] Read and scraped " + feeditems.length + " texts in " + (timeEnd - timeStart) + " ms" )

    timeStart = new Date().getTime()

    // folding best matches into matches would be more elegant
    matches ++= analyzer.bestMatches(feeditems, 100).takeRight(100)
    timeEnd = new Date().getTime()
    Console.println("[Info] Analyzed " + feeditems.length + " texts in " + (timeEnd - timeStart) + " ms" )
  }

  def serialize[T](fileName: String, t: T) {
    // For simplicity, use Java serialization to persist data between update runs
    // In the worst case, when the serialization format changes, the FeedMatcher
    // loses the best matches over the last days and starts fresh.
    val output = new ObjectOutputStream(new FileOutputStream(fileName))
    output.writeObject(t)
    output.close()
  }

  def unserialize[T](fileName: String) : Option[T] = try {
    val input = new ObjectInputStream(new FileInputStream(fileName))
    Some(input.readObject().asInstanceOf[T])
  } catch {
    case e: FileNotFoundException => None
  }

  import JSON._

  def main(args: Array[String]) {
    val analyzedFile = "tmp/koalition_analyzed.txt"
    val feeditemsSeenFile = "tmp/articles_seen.txt"
    val matchesFile = "tmp/matches.txt"
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

    // TODO: text fields contain markdown text => filter link urls
    val jsonString = io.Source.fromFile("test/koalitionsvertrag.json").getLines().mkString("\n")
    val json: JsValue = Json.parse(jsonString)
    val result: JsResult[Seq[InputStatement]] = json.validate[Seq[InputStatement]]

    initialize(result.get,
      io.Source.fromFile("test/top1000de.txt", "UTF-16"),
      analyzedFile
    )

    while(true) {
      val feeditemsSeen = unserialize[MapItemUrlLength](feeditemsSeenFile).getOrElse(
        new collection.mutable.HashMap[String, Int] with collection.mutable.SynchronizedMap[String, Int]
      )

      val bestMatches = unserialize[BestMatches](matchesFile).getOrElse(
        collection.mutable.SortedSet.empty[(Statement, TextMatch[AnalyzableItem])]
      )
      // TODO: Erase old matches from bestMatches

      val matchesNew = update(
        feeds,
        unserialize[Analyzer[Statement]](analyzedFile).get,
        /*in/out*/ feeditemsSeen,
        /*in/out*/ bestMatches
      )

      serialize(feeditemsSeenFile, feeditemsSeen)
      serialize(matchesFile, bestMatches)

      case class ResultMatch(title: String, url: String, confidence: Double, matched: Seq[String])
      case class Result(title: String, url: String, confidence: Double, articles: Seq[ResultMatch])

      implicit val matchWrites: Writes[ResultMatch] = (
        (JsPath \ "title").write[String] and
          (JsPath \ "url").write[String] and
          (JsPath \ "confidence").write[Double] and
          (JsPath \ "matched").write[Seq[String]]
        )(unlift(ResultMatch.unapply))

      implicit val resultWrites: Writes[Result] = (
        (JsPath \ "title").write[String] and
          (JsPath \ "url").write[String] and
          (JsPath \ "confidence").write[Double] and
          (JsPath \ "articles").write[Seq[ResultMatch]]
        )(unlift(Result.unapply))

      val matchesByStatement = bestMatches.
        groupBy(_._1).toArray. // by statement
        sortBy(_._2.last._2.value). // sort by highest match value
        reverse.map{
        case (stmt, setMatches) => {
          val bestMatches = setMatches.toList.reverse.slice(0,3).map(_._2).map( textmatch =>
            ResultMatch(
              textmatch.matched.title,
              textmatch.matched.link,
              textmatch.value,
              textmatch.words.map( t => t._1 + ":" + t._2)
            )
          )
          Result(stmt.title, stmt.url, bestMatches.head.confidence, bestMatches)
        }
      }

      val fileJSON = new FileWriter(jsonFile)
      val jsonMatches = Json.toJson(matchesByStatement)
      fileJSON.write(jsonMatches.toString())
      fileJSON.close()

      Thread.sleep(1000*60*10)
    }
    // TODO: Access Twitter timelines: https://dev.twitter.com/docs/api/1.1/get/statuses/user_timeline
    // TODO: Access ZEIT API:      key=94fb85c06355e4e27925c530cdba809fbfaf7d349202390264a4
  }
}
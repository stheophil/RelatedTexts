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

object FeedMatcher {
  class Statement(override val text: String, override val keywords: Seq[String], val url: String) extends Analyzable

  def initialize(file: String, stopwords: String, corpus: String, outName: String) {
    val analyzer = new Analyzer(
      (for(line <- io.Source.fromFile(file).getLines) yield
        new Statement(line, Seq.empty[String], "")
        ).toList,
      stopwords,
      Some(corpus)
    )
    serialize(outName, analyzer)
  }

  case class FeedItem(val item: Item, override val text: String) extends Analyzable

  type SeenArticles = collection.mutable.Map[String, (Int, Date)] // url, length, date to first seen time
  type BestMatches = collection.mutable.SortedSet[(Statement, TextMatch[FeedItem])]

  implicit val orderByMatchValue = Ordering.by[(Statement, TextMatch[FeedItem]), Double](_._2.value)

  def update(feeds: List[String], analyzer: Analyzer[Statement], articlesSeen: SeenArticles, matches: BestMatches) {
    import scala.concurrent._
    import scala.concurrent.ExecutionContext.Implicits.global

    var timeStart = new Date().getTime()
    val articles =
      Await.result(
        Future.sequence( feeds.map( url => {
          future {
            feed.Feed.newExtractedTexts(
              url,
              articlesSeen.get(_).map(_._1).getOrElse(0) // length of seen article or 0
            ).map(
                itemtxt => new FeedItem(itemtxt._1, itemtxt._2.text)
              )
          }
        })),
        duration.Duration(5, duration.MINUTES)
      ).flatten

    // add new articles to "seen" map
    articles.foreach( feedstat => {
      articlesSeen.update(feedstat.item.link, (feedstat.text.length, new Date()))
    })
    var timeEnd = new Date().getTime()
    Console.println("[Info] Read and scraped " + articles.length + " texts in " + (timeEnd - timeStart) + " ms" )

    timeStart = new Date().getTime()

    // folding best matches into matches would be more elegant
    matches ++= analyzer.bestMatches(articles, 100).takeRight(100)
    timeEnd = new Date().getTime()
    Console.println("[Info] Analyzed " + articles.length + " texts in " + (timeEnd - timeStart) + " ms" )
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

  def main(args: Array[String]) {
    val analyzedFile = "test/cdu_wahlversprechen_analyzed.txt"
    val articlesSeenFile = "test/articles_seen.txt"
    val matchesFile = "test/matches.txt"
    val jsonFile = "test/matches.json"

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

    initialize("test/cdu_wahlversprechen.txt",
      "test/stopwords.txt",
      "test/cdu_wahlprogramm.txt",
      analyzedFile
    )

    while(true) {
      val articlesSeen = unserialize[SeenArticles](articlesSeenFile).getOrElse(
        collection.mutable.Map.empty[String, (Int, Date)]
      )

      val bestMatches = unserialize[BestMatches](matchesFile).getOrElse(
        collection.mutable.SortedSet.empty[(Statement, TextMatch[FeedItem])]
      )
      // TODO: Erase old matches from bestMatches

      val matchesNew = update(
        feeds,
        unserialize[Analyzer[Statement]](analyzedFile).get,
        /*in/out*/ articlesSeen,
        /*in/out*/ bestMatches
      )

      // TODO: Resulting articlesSeen should only contain the links found in this update.
      // When articles disappear from RSS feed and then later reappear in the feed,
      // they are presumably updated

      serialize(articlesSeenFile, articlesSeen)
      serialize(matchesFile, bestMatches)

      val matchesByStatement = bestMatches.
        groupBy(_._1).toArray. // by statement
        sortBy(_._2.last._2.value). // sort by highest match value
        reverse

      val fileJSON = new FileWriter(jsonFile)
      fileJSON.append("{ matches : [ \n");
      matchesByStatement.foreach( {
        case (stmt, setMatches) => {
          fileJSON.append(" { \n")
          fileJSON.append(" title : \"\",\n")
          fileJSON.append(" text : \"" + stmt.text.replace('\"', '\'') + "\",\n")
          fileJSON.append(" url : \"" + stmt.url + "\",\n")
          val bestMatches = setMatches.toList.reverse.slice(0,3).map(_._2)
          fileJSON.append(" points : " + bestMatches.head.value + ",\n")

            fileJSON.append(" articles : { \n")
            bestMatches.foreach( textmatch => {
              fileJSON.append("\t{ \n")
              fileJSON.append("\ttitle : \"" + textmatch.matched.item.title.replace('\"', '\'') + "\",\n")
              fileJSON.append("\turl : \"" + textmatch.matched.item.link + "\",\n")
              fileJSON.append("\tpoints : " + textmatch.value + ",\n")
              fileJSON.append("\tmatched : \"" + textmatch.words.toString.filter(!_.equals('\"')) + "\"\n")

              if(bestMatches.last==textmatch) {
                fileJSON.append("\t} \n")
              } else {
                fileJSON.append("\t}, \n")
              }
            })

          if(matchesByStatement.last._1==stmt) {
            fileJSON.append("} \n")
          } else {
            fileJSON.append("}, \n")
          }
        }
      })
      fileJSON.append("]}");
      fileJSON.close()

      Thread.sleep(1000*60*10)
    }
    // TODO: Access Twitter timelines: https://dev.twitter.com/docs/api/1.1/get/statuses/user_timeline
    // TODO: Access ZEIT API:      key=94fb85c06355e4e27925c530cdba809fbfaf7d349202390264a4
  }
}
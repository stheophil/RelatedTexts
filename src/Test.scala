import _root_.text._
import feed._

import java.util.Date
import scala.Console

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
  def main(args: Array[String]) {
    class Statement(override val text: String, override val keywords: Seq[String], val url: String) extends Analyzable

    val analyzer = new Analyzer(
      (for(line <- io.Source.fromFile("test/cdu_wahlversprechen.txt").getLines) yield
      new Statement(line, Seq.empty[String], "")
      ).toList
    )

    val feeds = List("http://rss.sueddeutsche.de/rss/Politik",
      "http://www.welt.de/politik/deutschland/?service=Rss",
      "http://www.welt.de/wirtschaft/?service=Rss",
      "http://rss.sueddeutsche.de/rss/Wirtschaft",
      "http://newsfeed.zeit.de/politik/index",
      "http://newsfeed.zeit.de/wirtschaft/index",
      "http://newsfeed.zeit.de/gesellschaft/index",
      "http://www.faz.net/rss/aktuell/politik/",
      "http://www.faz.net/rss/aktuell/wirtschaft"
     )

    class FeedItem(val item: Item, override val text: String) extends Analyzable

    var articlesSeen = collection.mutable.Map.empty[String, (Int, Date)] // url, length, url to first seen time

    implicit val order = Ordering.by[(Statement, TextMatch[FeedItem]), Double](_._2.value)
    var matches = collection.mutable.SortedSet.empty[(Statement, TextMatch[FeedItem])]

    while(true) {
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

      // TODO: erase old articles from "seen" map

      timeStart = new Date().getTime()
      matches ++= analyzer.bestMatches(articles, 100)
      matches = matches.takeRight(100) // folding best matches into matches would be more elegant
      timeEnd = new Date().getTime()
      Console.println("[Info] Analyzed " + articles.length + " texts in " + (timeEnd - timeStart) + " ms" )

      matches.toSeq.reverse.foreach{
        case (entry, textmatch) => {
          Console.println("(" + textmatch.value +") Entry: " + entry.text)

          val feeditem = textmatch.matched
          Console.println("\t " + feeditem.item.title)
          Console.println("\t " + feeditem.item.link)
          Console.println("\t " + textmatch.words.sortBy(_._2).reverse.mkString(", "))
          Console.println("\t " + feeditem.text)
          Console.println("")
        }
      }

      Thread.sleep(1000*60*10)
    }
    // TODO: Access Twitter timelines: https://dev.twitter.com/docs/api/1.1/get/statuses/user_timeline
    // TODO: Access ZEIT API:      key=94fb85c06355e4e27925c530cdba809fbfaf7d349202390264a4
  }
}
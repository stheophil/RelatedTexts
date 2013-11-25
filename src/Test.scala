import _root_.text._
import feed._

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
    val analyzer = new Analyzer("test/cdu_wahlversprechen.txt")

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

    case class FeedStatistics(val item: Item, val text: String) extends TextStatistics(text, analyzer.stopWords)

    var articlesSeen = collection.mutable.Map.empty[String, (Int, java.util.Date)] // url, length, url to first seen time

    implicit val order = Ordering.by[(WeightedText, TextMatch[FeedStatistics]), Double](_._2.value)
    var matches = collection.mutable.SortedSet.empty[(WeightedText, TextMatch[FeedStatistics])]

    while(true) {
      import scala.concurrent._
      import scala.concurrent.ExecutionContext.Implicits.global

      val articles =
        Await.result(
          Future.sequence( feeds.map( url => {
            future {
              feed.Feed.newExtractedTexts(
                url,
                articlesSeen.get(_).map(_._1).getOrElse(0) // length of seen article or 0
              ).map(
                itemtxt => FeedStatistics(itemtxt._1, itemtxt._2.text)
              )
            }
          })),
          duration.Duration(5, duration.MINUTES)
        ).flatten

      // add new articles to "seen" map
      articles.foreach( feedstat => {
        articlesSeen.update(feedstat.item.link, (feedstat.text.length, new java.util.Date()))
      })

      // TODO: erase old articles from "seen" map

      matches ++= analyzer.bestMatches(articles, 100)
      matches = matches.takeRight(100) // folding best matches into matches would be more elegant
      matches.toSeq.reverse.foreach{
        case (entry, textmatch) => {
          Console.println("(" + textmatch.value +") Entry: " + entry.text)

          val feed = textmatch.text
          Console.println("\t " + feed.item.title)
          Console.println("\t " + feed.item.link)
          Console.println("\t " + textmatch.matches.sortBy(_._2).reverse.mkString(", "))
          Console.println("\t " + feed.text)
          Console.println("")
        }
      }

      Thread.sleep(1000*60*10)
    }
    // TODO: Access Twitter timelines: https://dev.twitter.com/docs/api/1.1/get/statuses/user_timeline
    // TODO: Access ZEIT API:      key=94fb85c06355e4e27925c530cdba809fbfaf7d349202390264a4
  }
}
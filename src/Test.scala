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
    import Text._

    // http://snowball.tartarus.org/algorithms/german/stop.txt
    val stopWords = io.Source.fromFile("test/stopwords.txt", "UTF-8").getLines().toList.flatMap(
      line => {
          val idxComment = line.indexOf("|")
          val text = line.substring(0, if(idxComment == -1) { line.length } else { idxComment }).trim
          if(text.isEmpty) {
            List.empty[String]
          } else {
            List(GermanStemmer(text))
          }
        }
      ).toSet

    val txtstatCorpus = new TextStatistics(
      Seq(
        io.Source.fromURL("http://www.gutenberg.org/cache/epub/34811/pg34811.txt", "UTF-8"), // Buddenbrocks
        io.Source.fromFile("test/cdu_wahlversprechen.txt", "UTF-8")
      ),
      stopWords)

    class WeightedText(val text: String, txtstatCorpus: TextStatistics) extends WeightedStatistics(text, txtstatCorpus, stopWords)
    val entries = (
      for(line <- io.Source.fromFile("test/cdu_wahlversprechen.txt").getLines) yield
        new WeightedText(line, txtstatCorpus)
      ).toList

    class FeedStatistics(val item: Item, val text: String) extends TextStatistics(text, stopWords)

    val feeds = List("http://rss.sueddeutsche.de/rss/Politik",
      "http://www.welt.de/politik/deutschland/?service=Rss",
      "http://www.welt.de/wirtschaft/?service=Rss",
      "http://rss.sueddeutsche.de/rss/Wirtschaft",
      "http://newsfeed.zeit.de/politik/index",
      "http://newsfeed.zeit.de/wirtschaft/index",
      "http://newsfeed.zeit.de/gesellschaft/index",
      "http://www.faz.net/rss/aktuell/politik/",
      "http://www.faz.net/rss/aktuell/wirtschaft")

    import scala.concurrent._
    import scala.concurrent.ExecutionContext.Implicits.global
    val articles =
      Await.result(
        Future.sequence( feeds.map( url => {
          future {
            val feed = new Feed(url)
            val itemdoc = feed.items. // TODO: Filter items we've seen already
              map( item => (item, item.paragraphs) ).
              filter( _._2.isSuccess ).
              map( itemdoc => (itemdoc._1, itemdoc._2.get) )

            val avgLength = itemdoc.map( _._2.text.length ).sum / itemdoc.length
            val shortArticle = (itemdoc: (Item, Document)) => itemdoc._2.text.length < avgLength/4

            itemdoc.filter( shortArticle ).foreach( itemdoc =>
              Console.println("[Warning] Document too short: " + itemdoc._1.link + " / " + itemdoc._2.text.length)
            )

            itemdoc.filter( !shortArticle(_) )
          }
        })),
        duration.Duration(5, duration.MINUTES)
      ).flatten.map( itemdoc => new FeedStatistics(itemdoc._1, itemdoc._2.text))

    val matches = bestMatches(entries, articles, 100).reverse.foreach{
      case (entry, score, textmatch) => {
        Console.println("(" + score +") Entry: " + entry.text)
        val feed = textmatch.text
        Console.println("\t " + feed.item.title)
        Console.println("\t " + feed.item.link)
        Console.println("\t " + textmatch.bestMatchingNGrams.sortBy(_._2).reverse.mkString(", "))
        Console.println("\t " + feed.text)
        Console.println("")
      }
    }

    // TODO: Access Twitter timelines: https://dev.twitter.com/docs/api/1.1/get/statuses/user_timeline
    // TODO: Access ZEIT API:      key=94fb85c06355e4e27925c530cdba809fbfaf7d349202390264a4
  }
}
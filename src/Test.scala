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

import org.jsoup.Jsoup
object RSS {
  class Item(val title : String, val link: String, val fulltext: String)

  def items(url: String) : Seq[Item] = {
    val feed = scala.xml.XML.load(url)
    feed.child.filter(_.label=="channel").flatMap(
      _.child.filter(_.label=="item").flatMap( item => {
            val opttitle = item.child.find(_.label == "title").map(_.text)
            val optlink = item.child.find( _.label == "link").map(_.text)
            val opttext = optlink map {
              link =>
                Console.println("Scrape " + link)

                try {
                  import scala.collection.JavaConversions._
                  val html = Jsoup.connect(link).get();
                  val text = html.select("p").map(_.text()).toList
                  val avgLength = text.map( _.length ).sum / text.length
                  // Console.println("Average <p> length: " + avgLength)

                  if(100<avgLength) {
                    /*
                    text.foreach( p => {
                      if(p.length<avgLength) {
                        Console.println("Ignored <p> below avg length: " + p)
                      }
                    })
                    */
                    text.filter(_.length >= avgLength).mkString(" ")
                  } else {
                    ""
                  }
                } catch {
                  case e: Exception =>
                    Console.println("Error fetching text. Exception: " + e.getMessage)

                  ""
                }
            }
            opttext match {
              case Some(text) => {
                val item = new Item(opttitle.getOrElse(""), optlink.get, text)
                List[Item](item)
              }
              case None => List.empty[Item]
            }
      }))
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
    Console.println("Stop Words: " + stopWords.mkString(", "))

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

    entries.foreach( entry => {
      Console.println("")
      Console.println("Entry: " + entry.text)
      entry.ngramWeights.foreach( mapseqWeight => {
        Console.println("")
        Console.println( mapseqWeight.head._1.length + "-gram weights: " +
          mapseqWeight.toArray.sortBy(_._2).reverse.map(
            t => (t._1.mkString(", "), t._2)
          ).mkString(", ")
        )
      })
    })

    class FeedStatistics(val item: RSS.Item) extends TextStatistics(item.fulltext, stopWords)

    val feeds = List("http://rss.sueddeutsche.de/rss/Politik",
      "http://www.welt.de/politik/deutschland/?service=Rss",
      "http://www.welt.de/wirtschaft/?service=Rss",
      "http://rss.sueddeutsche.de/rss/Wirtschaft",
      "http://newsfeed.zeit.de/politik/index",
      "http://newsfeed.zeit.de/wirtschaft/index",
      "http://newsfeed.zeit.de/gesellschaft/index",
      "http://www.faz.net/rss/aktuell/politik/",
      "http://www.faz.net/rss/aktuell/wirtschaft")

    val articles = feeds.flatMap( RSS.items(_) ).map( item => new FeedStatistics(item) )

    val matches = bestMatches(entries, articles, 100).reverse.foreach{
      case (entry, score, textmatch) => {
        Console.println("(" + score +") Entry: " + entry.text)
        Console.println("\t " + textmatch.text.item.title)
        Console.println("\t " + textmatch.text.item.link)
        Console.println("\t " + textmatch.bestMatchingNGrams.sortBy(_._2).reverse.mkString(", "))
        Console.println("\t " + textmatch.text.item.fulltext)
        Console.println("")
      }
    }

    // TODO: Access Twitter timelines: https://dev.twitter.com/docs/api/1.1/get/statuses/user_timeline
    // TODO: Access ZEIT API:      key=94fb85c06355e4e27925c530cdba809fbfaf7d349202390264a4
  }
}
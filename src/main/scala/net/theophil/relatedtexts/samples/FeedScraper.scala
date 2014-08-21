package net.theophil.relatedtexts.samples

import net.theophil.relatedtexts._
import java.io.{File, IOException, FileWriter}
import scala.util.{Try, Success, Failure}

/**
 * A simple application that parses a list of feeds and outputs both the original html
 * and the scraped text. Useful both for evaluating the scraper and for creating a
 * constant set of texts with which to evaluate the TextMatcher.
 */
object FeedScraper {
  def main(args: Array[String]): Unit = {
    val root = "/Users/sebastian/Dropbox/Programming/relatedtexts_testdata/"

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

    val html = new java.io.File(root + "original_html")
    if(!html.exists() && !html.mkdir()) {
      println("Error creating folder " + html.getName)
      sys.exit(1)
    }

    val scraped = new java.io.File(root + "scraped")
    if(!scraped.exists() && !scraped.mkdir()) {
      println("Error creating folder " + scraped.getName)
      sys.exit(1)
    }

    def write(folder: File, filename: String, content: String): Unit = {
      var writer : FileWriter = null
      val name = folder.getAbsolutePath() + "/" + filename
      try {
        writer = new FileWriter(name)
        writer.write(content)
      } catch {
        case e : IOException => {
          println("Error writing file " + name)
          e.printStackTrace()
        }
      } finally {
        if(writer != null) writer.close()
      }
    }

    var i = 0
    feeds.foreach( f => {
      val mapUrlToLength = FeedMatcherCache.newMapUrlToLength
      val items = Feed.newItemsWithText(
        f,
        (s: String) => 0, // items never seen before -> previous length is 0
        mapUrlToLength)

      items.foreach(item => {
        try {
          val original = io.Source.fromURL(item.link, "UTF-8")
          write(html, i + ".html", original.getLines().mkString("\n"))
          write(scraped, i + ".txt", item.text)
          i += 1
        } catch {
          case e: Exception => {
            println("Error reading from URL " + item.link)
            e.printStackTrace()
          }
        }
      })
    })
  }
}

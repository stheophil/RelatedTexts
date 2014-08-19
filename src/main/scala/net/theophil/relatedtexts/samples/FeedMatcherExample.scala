package net.theophil.relatedtexts.samples

import net.theophil.relatedtexts.FeedMatcherCache.AnalyzableItem
import net.theophil.relatedtexts._
import java.io.{FileOutputStream, OutputStreamWriter}
import java.nio.charset.Charset
import play.api.libs.json._

object FeedMatcherTest {
  import JSON._

  def removeMarkdownLink(text: String) : String = {
    val markdownLink = """\[([^\]]*)\]\(([^\)]*)\)""".r("text", "href")
    markdownLink.replaceAllIn( text, _ group "text" )
  }

  def main(args: Array[String]) {
    // The RSS feeds to parse
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

    // Load a set of texts to match against content of feeds
    val jsonString = io.Source.fromInputStream(
      getClass.getResourceAsStream("/test_data/koalitionsvertrag.json"),
      "UTF-8"
    ).getLines().mkString("\n")

    val json: JsValue = Json.parse(jsonString)
    val seqInputs: JsResult[Seq[InputText]] = json.validate[Seq[InputText]]

    val texts = seqInputs.get.map( input => {
      DefaultText(
        removeMarkdownLink(input.title),
        removeMarkdownLink(input.title + " " + input.quote),
        input.tags,
        "http://www.wahlversprechen2013.de/item/" + input.id
      )
    })

    // FeedMatcher will read all the feeds, scrape the articles and will
    // return the elements of text that were most closely
    // related statistically to the articles referenced by the RSS feeds.
    val resultlist = new ResultListBuilder[DefaultText, AnalyzableItem]
    FeedMatcher(
      texts,
      feeds,
      (m: (DefaultText, TextMatch[AnalyzableItem])) => resultlist(m),
      /*limit*/ 4.0
    )

    Console.println("Writing best matches to tmp/matches.json");
    val fos = new FileOutputStream("tmp/matches.json")
    val fileJSON = new OutputStreamWriter(fos, Charset.forName("UTF-8"))

    val jsonMatches = Json.toJson(resultlist.toIterable)
    fileJSON.write("var matches = " + jsonMatches.toString())

    fileJSON.close()
    fos.close()
  }
}
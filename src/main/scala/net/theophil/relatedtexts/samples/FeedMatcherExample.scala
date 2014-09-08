package net.theophil.relatedtexts.samples

import net.theophil.relatedtexts.FeedMatcherCache.AnalyzableItem
import net.theophil.relatedtexts._
import play.api.libs.json._

object FeedMatcherExample {
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

    // FeedMatcher will read all the feeds, scrape the articles and will
    // return the elements of text that were most closely
    // related statistically to the articles referenced by the RSS feeds.
    val resultlist = new ResultListBuilder[DefaultText, AnalyzableItem]
    FeedMatcher(
      samples.loadSampleTexts,
      feeds,
      (m: (DefaultText, TextMatch[AnalyzableItem])) => resultlist(m),
      /*limit*/ 4.0
    )

    writeMatches(Json.toJson(resultlist.toIterable).toString())
  }
}
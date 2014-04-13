package net.theophil.relatedtexts

import scala.util.matching.Regex.Match
import java.util.Date

case class FeedMatcherCache(analyzer: Analyzer[Statement], mapUrlToLength: MapUrlToLength, matches: SetBestMatches) extends Serializable {
  def serialize(fileName: String) {
    // For simplicity, use Java serialization to persist data between update runs
    // In the worst case, when the serialization format changes, the FeedMatcher
    // loses the best matches over the last days and starts fresh.
    val output = new java.io.ObjectOutputStream(new java.io.FileOutputStream(fileName))
    output.writeObject(this)
    output.close()
  }
}

object FeedMatcherCache {
  def fromFile(fileName: String) : Option[FeedMatcherCache] = try {
    val input = new java.io.ObjectInputStream(new java.io.FileInputStream(fileName))
    Some(input.readObject().asInstanceOf[FeedMatcherCache])
  } catch {
    case e: java.io.FileNotFoundException => None
  }
}

object FeedMatcher {
  def apply(feeds: Seq[String], statements: Seq[Statement], count: Int, cache: Option[FeedMatcherCache]) : (Seq[Result], FeedMatcherCache) = {
    val analyzer = cache.map(_.analyzer).getOrElse( new Analyzer(
      statements,
      io.Source.fromFile("test/top1000de.txt", "UTF-16")
    ))

    val mapUrlToLengthLastRun = cache.map(_.mapUrlToLength).getOrElse( newMapUrlToLength )
    val mapUrlToLength = newMapUrlToLength

    val matches = cache.map(_.matches).getOrElse( new SetBestMatches )

    import scala.concurrent._
    import scala.concurrent.ExecutionContext.Implicits.global

    var timeStart = new Date().getTime()
    val feeditems =
      Await.result(
        Future.sequence( feeds.map( url => {
          future {
            Feed.newItemsWithText(
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
    matches ++= analyzer.bestMatches(feeditems, count).takeRight(count)
    timeEnd = new Date().getTime()
    Console.println("[Info] Analyzed " + feeditems.length + " texts in " + (timeEnd - timeStart) + " ms" )

    val resultsByStatement = matches.
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

    (resultsByStatement, FeedMatcherCache(analyzer, mapUrlToLength, matches))
  }
}

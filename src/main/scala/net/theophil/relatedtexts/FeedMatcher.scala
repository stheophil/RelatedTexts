package net.theophil.relatedtexts

import scala.util.matching.Regex.Match
import java.util.Date

case class FeedMatcherCache[T <: Analyzable](analyzer: Analyzer[T], mapUrlToLength: MapUrlToLength, matches: SetBestMatches[T]) extends Serializable {
  def serialize(fileName: String) {
    // For simplicity, use Java serialization to persist data between update runs
    // In the worst case, when the serialization format changes, the FeedMatcher
    // loses the best matches over the last days and starts fresh.
    var file : java.io.FileOutputStream = null
    var output : java.io.ObjectOutputStream = null
    try {
      file = new java.io.FileOutputStream(fileName)
      output = new java.io.ObjectOutputStream(file)
      output.writeObject(this)
    } catch {
      case e: Exception => e.printStackTrace()
    } finally {
      if(output!=null) output.close()
      if(file!=null) file.close()
    }
  }
}

object FeedMatcherCache {
  def fromFile[T <: Analyzable](fileName: String): Option[FeedMatcherCache[T]] = {
    var file: java.io.FileInputStream = null
    var input: java.io.ObjectInputStream = null
    try {
      file = new java.io.FileInputStream(fileName)
      input = new java.io.ObjectInputStream(file)

      Some(input.readObject().asInstanceOf[FeedMatcherCache[T]])
    } catch {
      case e: java.io.FileNotFoundException => None
      case e: Exception => {
        e.printStackTrace()
        None
      }
    } finally {
      if(input!=null) input.close()
      if(file!=null) file.close()
    }
  }
}

object FeedMatcher {
  def apply[T <: Analyzable](feeds: Seq[String], texts: Seq[T], count: Int, cache: Option[FeedMatcherCache[T]]) : (Seq[Result[T]], FeedMatcherCache[T]) = {
    val analyzer = cache.map(_.analyzer).getOrElse( new Analyzer(
      texts,
      io.Source.fromInputStream(getClass.getResourceAsStream("/de/top1000de.txt"), "UTF-16")
    ))

    val mapUrlToLengthLastRun = cache.map(_.mapUrlToLength).getOrElse( newMapUrlToLength )
    val mapUrlToLength = newMapUrlToLength

    val matches = cache.map(_.matches).getOrElse( new SetBestMatches[T] )

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
      case (text, setMatches) => {
        val bestMatches = setMatches.toList.reverse.slice(0,3).map(_._2).map( textmatch =>
          ResultMatch(
            textmatch.matched.title,
            textmatch.matched.link,
            textmatch.value,
            textmatch.words.map( t => t._1 + ":" + t._2)
          )
        )
        Result(text, bestMatches.head.confidence, bestMatches)
      }
    }

    (resultsByStatement, FeedMatcherCache[T](analyzer, mapUrlToLength, matches))
  }
}

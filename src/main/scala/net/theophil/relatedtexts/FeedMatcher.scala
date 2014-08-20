package net.theophil.relatedtexts

import java.util.Date

import scala.collection.mutable

/**
 * A serialization helper object used to serialize objects to and from a file with correct error handling.
 */
object Cache {
  def fromFile[T](fileName: String): Option[T] = {
    var file: java.io.FileInputStream = null
    var input: java.io.ObjectInputStream = null
    try {
      file = new java.io.FileInputStream(fileName)
      input = new java.io.ObjectInputStream(file)

      Some(input.readObject().asInstanceOf[T])
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

  def toFile(fileName: String, o: Object) {
    // For simplicity, use Java serialization to persist data between update runs
    // In the worst case, when the serialization format changes, the FeedMatcher
    // loses the best matches over the last days and starts fresh.
    var file : java.io.FileOutputStream = null
    var output : java.io.ObjectOutputStream = null
    try {
      file = new java.io.FileOutputStream(fileName)
      output = new java.io.ObjectOutputStream(file)
      output.writeObject(o)
    } catch {
      case e: Exception => e.printStackTrace()
    } finally {
      if(output!=null) output.close()
      if(file!=null) file.close()
    }
  }
}

/**
 * Helper class to build an [[Iterable]] of [[Result]][S,T]
 * @tparam S
 * @tparam T
 */
class ResultListBuilder[S <: Analyzable, T <: Analyzable] {
  val matches = mutable.Map.empty[S, mutable.ArrayBuffer[TextMatch[T]]]

  /**
   * Add a match to the [[Iterable]] being built
   * @param m the match returned from the [[TextMatcher]] or [[FeedMatcherCache]]
   */
  def apply(m : (S, TextMatch[T])) {
    var buffer = matches.getOrElseUpdate(m._1, mutable.ArrayBuffer.empty[TextMatch[T]])
    buffer += m._2
  }

  /**
   * Build an [[Iterable]] from the collected matches, sorted by highest score in descending order
   * @return
   */
  def toIterable : Iterable[Result[S, T]] = {
    matches.values.foreach(_.sortBy(_.value)(Ordering[Double].reverse))

    val bufferSorted = matches.to[mutable.ArrayBuffer].sortBy(_._2.head.value)(Ordering[Double].reverse)
    bufferSorted.view.map {
      case (text, bufferMatches) => {
        val seqResultMatch = bufferMatches.map( textmatch =>
          ResultMatch[T](
            textmatch.matched,
            textmatch.value,
            textmatch.words.map( t => t._1 + ":" + t._2)
          )
        )
        Result[S, T](text, seqResultMatch.head.confidence, seqResultMatch)
      }
    }
  }
}

/**
 * The [[TextMatcher]] compares two sets of analyzable texts.
 */
object TextMatcher {
  /**
   * Compare two sets of texts and pass the lists of matches with a higher score than `limit` to `fn`.
   * The results of preprocessing `textsLeft` is cached. The `textsLeft` can thus be compared to
   * different `textsRight` without preprocessing `textsLeft` repeatedly.
   * @param textsLeft the texts whose preprocessing needs to be done only once
   * @param textsRight the texts to match against, will be preprocessed on every call
   * @param fn the function to call for each match with a score higher than `limit`
   * @param limit the minimum score a matching needs to be returned
   * @param analyzerCached the optional [[Analyzer]] containing the preprocessed `textsLeft`
   * @return the [[Analyzer]] containing the preprocessed `textsLeft`. This object may be serialized
   *         e.g. with the [[Cache]] helper and can be passed to the next call to `apply` to avoid
   *         preprocessing the same texts twice.
   */
  def apply[S <: Analyzable, T <: Analyzable](textsLeft: Seq[S],
                                              textsRight: Seq[T],
                                              fn : ((S, TextMatch[T])) => Unit,
                                              limit: Double = 0.0,
                                              analyzerCached: Option[Analyzer[S]] = None)
  : Analyzer[S] =
  {
    val analyzer = analyzerCached.getOrElse( preprocess(textsLeft) )
    val timeStart = new Date().getTime()

    analyzer.foreach(textsRight, limit)(fn)

    val timeEnd = new Date().getTime()
    Console.println("[Info] Analyzed " + textsRight.length + " texts in " + (timeEnd - timeStart) + " ms" )
    analyzer
  }

  /**
   * Compare two sets of texts and return the lists of matches with a higher score than `limit`.
   * The results of preprocessing `textsLeft` is cached. The `textsLeft` can thus be compared to
   * different `textsRight` without preprocessing `textsLeft` repeatedly.
   * @param textsLeft the texts whose preprocessing needs to be done only once
   * @param textsRight the texts to match against, will be preprocessed on every call
   * @param limit the minimum score a matching needs to be returned
   * @param analyzerCached the optional [[Analyzer]] containing the preprocessed `textsLeft`
   * @return a pair containing an [[Iterable]] of [[Result]]s which contains the matching texts grouped by
   *         the texts in `textsLeft` and an [[Analyzer]] that contains the preprocessed `textsLeft`. The
   *         [[Analyzer]] can be serialized e.g. using the helper [[Cache]] object and can be passed to
   *         the next `apply` call to avoid preprocessing `textsLeft` twice.
   */
  def apply[S <: Analyzable, T <: Analyzable](textsLeft: Seq[S],
                                              textsRight: Seq[T],
                                              limit: Double = 0.0,
                                              analyzerCached: Option[Analyzer[S]] = None)
  : (Iterable[Result[S, T]], Analyzer[S]) =
  {
    val resultlist = new ResultListBuilder[S, T]

    val analyzer = TextMatcher.apply[S, T](
      textsLeft,
      textsRight,
      resultlist(_),
      limit,
      analyzerCached
    )

    (resultlist.toIterable, analyzer)
  }

  /**
   * Create the [[Analyzer]] object containing the preprocessed `textsLeft`
   * @param textsLeft The texts to be preprocessed
   * @tparam S
   * @return
   */
  def preprocess[S <: Analyzable](textsLeft: Seq[S]) = new Analyzer(
    textsLeft,
    io.Source.fromInputStream(getClass.getResourceAsStream("/de/top1000de.txt"), "UTF-16")
  )
}

/**
 * Helper object to class [[FeedMatcherCache]] containing type definitions.
 */
object FeedMatcherCache {
  /**
   * Mutable [[scala.collection.mutable.HashMap]] that maps feed URLs to result lengths and is used to
   * track which feed URLs have been read on the last run.
   */
  type MapUrlToLength = collection.mutable.HashMap[String, Int] with collection.mutable.SynchronizedMap[String, Int]

  def newMapUrlToLength: MapUrlToLength = {
    new collection.mutable.HashMap[String, Int] with collection.mutable.SynchronizedMap[String, Int]
  }

    /**
     * An [[Item]] of a RSS feed extending [[Analyzable]].
     */
  type AnalyzableItem = Item with Analyzable
}
import FeedMatcherCache._

/**
 * When matching RSS feed items against texts, not only the preprocessed texts can be serialized and cached
 * as in the [[TextMatcher]], but the list of feed [[Item]]s seen on the last run should be stored too.
 * @param analyzer The [[Analyzer]] containing the preprocessed texts
 * @param mapUrlToLength The map used to check if an RSS feed item has been read and parsed on the last run.
 * @tparam S
 */
@SerialVersionUID(1l)
case class FeedMatcherCache[S <: Analyzable](analyzer: Analyzer[S],
                                             mapUrlToLength: MapUrlToLength) extends Serializable

object FeedMatcher {
  /**
   * Compare a list of analyzable texts to the texts scraped from web pages referenced in RSS feeds
   * @param texts the [[Analyzable]] texts
   * @param feeds the list of RSS feed urls. The feeds will be parsed sequentially, the items in each feed
   *              are parsed in parallel
   * @param limit the minimum matching score
   * @param cache the optional [[FeedMatcherCache]] returned from a previous call to [[apply]]. Contains the
   *              preprocessed `texts` and the information which feed items have been scraped already.
   * @tparam S
   * @return An updated [[FeedMatcherCache]] object
   */
  def apply[S <: Analyzable](texts: Seq[S],
                             feeds: Seq[String],
                             fn: ((S, TextMatch[AnalyzableItem])) => Unit,
                             limit : Double = 0.0,
                             cache: Option[FeedMatcherCache[S]] = None) : FeedMatcherCache[S] =
  {
    val mapUrlToLengthLastRun = cache.map(_.mapUrlToLength).getOrElse( newMapUrlToLength )
    val mapUrlToLength = newMapUrlToLength

    val analyzer = cache.map( _.analyzer ).getOrElse(TextMatcher.preprocess(texts))

    val timeStart = new Date().getTime()
    feeds.foreach{ feedurl =>
      analyzer.foreach(
        Feed.newItemsWithText(
          feedurl,
          mapUrlToLengthLastRun.get(_).getOrElse(0), // length of seen article or 0
          /* out */ mapUrlToLength
        ).map(
            item => new Item(item.title, item.link, item.text, item.faviconUrl) with Analyzable
        ),
        limit
      )(fn)
    }
    val timeEnd = new Date().getTime()
    Console.println("[Info] Read, scraped and analyzed " + feeds.length + " feeds in " + (timeEnd - timeStart) + " ms" )

    FeedMatcherCache[S](analyzer, mapUrlToLength)
  }
}

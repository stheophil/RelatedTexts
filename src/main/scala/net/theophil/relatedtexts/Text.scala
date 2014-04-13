package net.theophil.relatedtexts

class TextStatistics(val ngramCount: Map[String, Int], val keywords: Set[String]) extends Serializable {
  def apply(ngram: String) : Int = {
    ngramCount.getOrElse(ngram, 0)
  }

  def weight(ngram: String) : Double = {
    // Using 1/word count seems too strict. The value of more frequent words should fall off slower.
    // Words that occur only 1, 2 or 10 times in such long texts should probably be worth roughly the same.
    // A linear decreasing function however just amplified the noise
    // So did (1/count) + linear function
    1.0 / Math.max(apply(ngram), 1)
  }

  def maxWeight : Double = 1.0
}

object TextStatistics {
  def apply(text: String, keywords: Seq[String], stopwords: Set[String]) : TextStatistics = {
    val regexSeparator = "[^\\wÄÖÜäöüß]+"

    val words = text.
      split(regexSeparator).
      map(s => GermanStemmer(s)).
      filter( stem => !stopwords(stem) && !stem.forall(_.isDigit) )

    val maxNGram = 3

    val ngramCount = (for(i <- 1 to maxNGram) yield {
      words.sliding(i).foldLeft(Map.empty[String, Int]) {
        (map, seqwords) => map + (seqwords.mkString(" ") -> (map.getOrElse(seqwords.mkString(" "), 0) + 1))
      }
    }).flatten.toMap

    new TextStatistics(ngramCount, keywords.map( GermanStemmer(_)).toSet)
  }
}

trait Analyzable {
  def text: String
  def keywords: Seq[String] = Seq.empty[String]
}

case class TextMatch[B](val value: Double, val words: Seq[(String, Double)], val matched: B)

class Analyzer[T<:Analyzable](analyzables: Seq[T], stopwordsFile: io.Source) extends Serializable {
  // http://snowball.tartarus.org/algorithms/german/stop.txt
  val stopwords = stopwordsFile.getLines().toList.flatMap(
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

  val statistics = analyzables.map( a => (a, TextStatistics(a.text, a.keywords, stopwords)) )
  val statisticsGlobal = new TextStatistics(
    statistics.map( _._2.ngramCount.keys ).
      flatten.
      foldLeft(Map.empty[String, Int]){
      case (map, str) => {
        map + (str -> (map.getOrElse(str, 0) + 1))
      }
    },
    Set.empty[String])

  def score(statThis: TextStatistics, statOther: TextStatistics) : (Double, Seq[(String, Double)]) = {
      val ngramCountWithGlobal = statThis.ngramCount.map{ case(str, count) => {
        (str -> (count, statisticsGlobal(str)))
      }}.toMap ++
        statThis.keywords.map( // keywords override ngramCount
          k => (k -> (statThis.ngramCount.getOrElse(k, 1), 1))
        ).toMap

     val matchingNgrams = ngramCountWithGlobal.filter{
        case (str, (count, globalCount)) => 0<statOther(str)
      }

      // longer ngrams don't have a higher score themselves because a 2-gram usually implies
      // two 1-grams too

      // TODO: Normalize by number of ngrams in statThis?
      // TODO: multiply with log(count) instead?
      val scores = matchingNgrams.map{
        case (str, (count, globalCount)) => (str, count * 1.0 / Math.max(1, globalCount) )
      }.toSeq
      (scores.map(_._2).sum, scores)
  }

  def bestMatches[B<:Analyzable](texts: Seq[B], limit: Int) : Seq[(T, TextMatch[B])] = {
    val statisticsOther = texts.map( text => (text, TextStatistics(text.text, text.keywords, stopwords)))

    implicit val order = Ordering.by[(T, TextMatch[B]), Double](_._2.value)

    var queue = scala.collection.immutable.SortedSet.empty[(T, TextMatch[B])]
    statisticsOther.foreach( textstat => {
      statistics.foreach( textstatThis => {
        val s = score(textstatThis._2, textstat._2)
        val element = (textstatThis._1, new TextMatch[B](s._1, s._2, textstat._1))
        if(queue.size < limit) {
          queue = queue + element
        } else if(order.lt(queue.head, element)) {
          queue = (queue + element) - queue.head
        }
      })
    })

    // Grouping the 'limit' best matches e.g. by entry and _adding_ the scores is not a good idea
    // The quality of matches declines rapidly. Some bad matches may occur very often which
    // can in the end overshadow the good matches.
    queue.toList
  }
}
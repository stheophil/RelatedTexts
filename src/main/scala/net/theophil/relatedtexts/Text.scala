package net.theophil.relatedtexts

@SerialVersionUID(1l)
case class NGramCount(local: Int, global: Int)

@SerialVersionUID(2l)
class TextStatistics(val ngramsWithCount: Map[String, NGramCount], val keywords: Set[String]) extends Serializable {
  def apply(ngram: String) : NGramCount = {
    ngramsWithCount.getOrElse(ngram, NGramCount(0,0))
  }
}

object TextStatistics {
  def ngramsAndKeywords(text: String, keywords: Seq[String], stopwords: Set[String]) : (Map[String, Int], Set[String]) = {
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

   (ngramCount, keywords.map( GermanStemmer(_)).toSet)
  }

  def withoutGlobalCount(text: String, keywords: Seq[String], stopwords: Set[String]) = {
    val ngramKeywords = ngramsAndKeywords(text, keywords, stopwords)
    new TextStatistics(
      ngramKeywords._1.map( t => (t._1 -> NGramCount(t._2, 0))),
      ngramKeywords._2
    )
  }
}

trait Analyzable {
  def text: String
  def keywords: Seq[String] = Seq.empty[String]
}

case class TextMatch[T<:Analyzable](val value: Double, val words: Seq[(String, Double)], val matched: T)

@SerialVersionUID(2l)
class Analyzer[S<:Analyzable](val statistics: Seq[(S, TextStatistics)],
                              val ngramsWithGlobalCounts : Map[String, Int],
                              val stopwords: Set[String]) extends Serializable {

  def score(statThis: TextStatistics, statOther: TextStatistics) : (Double, Seq[(String, Double)]) = {
    // 1. Version: Matching word scored with "local count / global count",
    // keywords always scored with 1.0
    // On test set: 187 matches with score > 4 with avg quality 0.31
    // 2. Version: Matching word scored with "1 / global count"
    // On test set: 38 matches with score > 4 with avg quality 0.70 (matches_ignore_count.json)
    // 3. Version: Score keywords with document freq. & and filter with stopword list
    // On test set: 27 matches with score > 4 with avg quality 0.80 (matches_filter_keywords.json)
    // 4. Version: Divide score by log(maximum possible score)
    // On test set: 27 matches with score > 1.0 with avg quality 0.88 (matches_div_logmaxscore.json)
    val ngramsKeywordsWithCount = statThis.ngramsWithCount ++
      statThis.keywords.filter{
        str => !stopwords.contains(str)
      }.map{
          str => (str, NGramCount(1, ngramsWithGlobalCounts.getOrElse(str, 1)))
      }

    val matchingNgramsKeywords = ngramsKeywordsWithCount.filter{
      case (str, count) => 0<statOther(str).local
    }

    // longer ngrams don't have a higher score themselves because a 2-gram usually implies
    // two 1-grams too

    val scores = matchingNgramsKeywords.map{
       case (str, count) => (str, 1.0 / Math.max(1, count.global).toDouble )
    }.toSeq

    val max_score = ngramsKeywordsWithCount.map{
      case (str, count) => 1.0 / Math.max(1, count.global).toDouble
    }.sum

    (scores.map(_._2).sum / Math.log(max_score), scores)
  }

  def foreach[T<:Analyzable](texts: Seq[T], limit: Double)(fn: ((S, TextMatch[T])) => Unit) {
     val statisticsOther = texts.map( text => (
      text,
      TextStatistics.withoutGlobalCount(text.text, text.keywords, stopwords)
    ))

    statisticsOther.foreach( textstat => {
      statistics.foreach( textstatThis => {
        val s = score(textstatThis._2, textstat._2)
        if( limit <= s._1 ) {
          fn( (textstatThis._1, TextMatch[T](s._1, s._2, textstat._1)) )
        }
      })
    })
  }
}

object Analyzer {
  def apply[S<:Analyzable](analyzables: Seq[S], stopwordsFile: io.Source) = {

    // Parse list of stopwords, one stop word per line
    // Rest of line after | is considered a comment
    // Short list of stopwords e.g.: http://snowball.tartarus.org/algorithms/german/stop.txt
    // Better: Use 1k most frequent words
    val stopwords = stopwordsFile.getLines().toList.flatMap(
      line => {
        val idxComment = line.indexOf("|")
        val text = line.substring(0, if (idxComment == -1) {
          line.length
        } else {
          idxComment
        }).trim
        if (text.isEmpty) {
          List.empty[String]
        } else {
          List(GermanStemmer(text))
        }
      }
    ).toSet

    // Calculate ngram counts in each analyzable and stemmed keywords
    val statistics : Seq[(S, Map[String, Int], Set[String])] = analyzables.map(a => {
      val ngramsKeywords = TextStatistics.ngramsAndKeywords(a.text, a.keywords, stopwords)
      (a, ngramsKeywords._1, ngramsKeywords._2)
    })

    // Calculate global ngram counts
    val ngramsWithGlobalCounts = statistics.map(_._2.keys).
        flatten.
        foldLeft(Map.empty[String, Int]) {
        case (map, str) => {
          val count = map.getOrElse(str, 0)
          map + (str -> (count+1))
        }
      }

    // Create final Analyzer
    new Analyzer[S](
      statistics.map{
        case (analyzable, ngramsWithCount, keywords) => {
          val ngramsWithGlobalCount = ngramsWithCount.map{
            case (ngram, count) => {
              (ngram -> NGramCount(count, ngramsWithGlobalCounts(ngram)))
            }
          }
          (analyzable, new TextStatistics(ngramsWithGlobalCount, keywords))
        }
      },
      ngramsWithGlobalCounts,
      stopwords
    )
  }

}

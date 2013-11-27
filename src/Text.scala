package text {

  class TextStatistics(text: String, stopwords: Set[String]) {
    val regexSeparator = "[^\\wÄÖÜäöüß]+"

    val ngramCount = countNGrams(text.
      split(regexSeparator).
      map(s => GermanStemmer(s)).
      filter( !stopwords(_) ),
      3) // max n-gram = 2

    def apply(ngram: Seq[String]) : Int = {
      require(0 < ngram.length )
      require(ngram.length <= ngramCount.length)
      ngramCount(ngram.length-1).getOrElse(ngram, 0)
    }

    def weight(ngram: Seq[String]) : Double = {
      // Using 1/word count seems too strict. The value of more frequent words should fall off slower.
      // Words that occur only 1, 2 or 10 times in such long texts should probably be worth roughly the same.
      // A linear decreasing function however just amplified the noise
      // So did (1/count) + linear function
      1.0/Math.max(apply(ngram), 1)
    }

    def maxWeight : Double = 1.0

    private def countNGrams(words: Seq[String], n: Int) : IndexedSeq[Map[Seq[String], Int]] = {
      // TODO: Build IndexedSeq[SortedMap[Seq[String], Int]] here. Counting text matches
      // is calculating intersection of two maps.
      require(words.isTraversableAgain)
      (for(i <- 1 to n) yield {
        words.sliding(i).foldLeft(Map.empty[Seq[String], Int]) {
          (map, seqwords) => map + (seqwords -> (map.getOrElse(seqwords, 0) + 1))
        }
      }).toIndexedSeq
    }
  }

  trait Analyzable {
    def text: String
    def keywords: Seq[String] = Seq.empty[String]
  }

  class WeightedStatistics[T<:Analyzable](val analyzable: T, txtstatGlobal: TextStatistics, stopwords: Set[String] ) {
    private val keywordStems = analyzable.keywords.map(GermanStemmer(_)).toSet

    // Currently, matches are not worth more the longer the ngram.
    // Since every word in a matching 2-gram also matches on its own, 2-grams are "worth more" automatically
    val ngramWeights = new TextStatistics(analyzable.text, stopwords).ngramCount.toSeq.map(
      _.map{
          case (seqwords, count) => {
            if(seqwords.length==1 && keywordStems(seqwords.head))
              (seqwords, txtstatGlobal.maxWeight)
            else
              (seqwords, txtstatGlobal.weight(seqwords))
          }
      }
    )

    def score(statsOther: TextStatistics) : (Double, Seq[(String, Double)]) = {
      // We ignore how often a word occurs in statsOther when computing the score.
      // Too often, a single word may occur very often in a long enough text.
      // What should count is the total number of (relevant) words that occur in a text.
      // The relative number of matching words (relative to total number of words in text)
      // would disadvantage longer texts.
      // Therefore we still calculate absolute value of matches.
      val ngramScores = ngramWeights.map(
        _.map {
          case (seqwords, weight) => (seqwords, Math.min(statsOther(seqwords), 1.0) * weight)
        }.toList.filter(_._2>0)
      )

      (
        ngramScores.foldLeft(0.0) {
          case (sum, listSeqScore) => sum + listSeqScore.map(_._2).sum
        },
        ngramScores.flatten.
          sortBy(_._2).
          takeRight(5).map{
          case (seqstr, f) => ("\"" + seqstr.mkString(" ") + "\"", f)
        }
      )
    }
  }
  class TextMatch[B](val value: Double, val words: Seq[(String, Double)], val matched: B)

  class Analyzer[T<:Analyzable](analyzables: Seq[T]) {
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

    val corpus = new TextStatistics(
        (io.Source.
          fromURL("http://www.gutenberg.org/cache/epub/34811/pg34811.txt", "UTF-8").
          getLines() ++ // Buddenbrocks
          analyzables.map(_.text) ++
          analyzables.flatMap(_.keywords)).mkString("\n"),
      stopWords)

    val entries = analyzables.map( new WeightedStatistics[T](_, corpus, stopWords) )

    def bestMatches[B<:Analyzable](texts: Seq[B], limit: Int) : Seq[(T, TextMatch[B])] = {
      val textstats = texts.map( text => (text, new TextStatistics(text.text, stopWords)))

      implicit val order = Ordering.by[(T, TextMatch[B]), Double](_._2.value)
      var queue = scala.collection.immutable.SortedSet.empty[(T, TextMatch[B])]
      textstats.foreach( textstat => {
        entries.foreach( entry => {
          val score = entry.score(textstat._2)
          val element = (entry.analyzable, new TextMatch[B](score._1, score._2, textstat._1))
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
}

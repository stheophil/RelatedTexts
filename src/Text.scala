object Text {

  val regexSeparator = "[^\\wÄÖÜäöüß]+"
  class TextStatistics(text: String, stopwords: Set[String]) {
    val ngramCount = countNGrams(text.
      split(regexSeparator).
      map(s => GermanStemmer(s)).
      filter( !stopwords(_) ),
      2) // max n-gram = 2

    def this(seqfile: Seq[io.Source], stopwords: Set[String]) {
      this( seqfile.flatMap(_.getLines()).mkString("\n"), stopwords )
    }

    def apply(ngram: Seq[String]) : Int = {
      require(0 < ngram.length )
      require(ngram.length <= ngramCount.length)
      ngramCount(ngram.length-1).getOrElse(ngram, 0)
    }

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

  class Score(val value: Double, val matches: Seq[(Seq[String], Double)])

  class WeightedStatistics(text: String, txtstatGlobal: TextStatistics, stopwords: Set[String] ) {
    // Currently, matches are not worth more the longer the ngram.
    // Since every word in a matching 2-gram also matches on its own, 2-grams are "worth more" automatically

    // TODO: Using 1/word count seems too strict. The value of more frequent words should fall off slower.
    // Words that occur only 1, 2 or 10 times in such long texts should probably be worth roughly the same.
    // The linear decreasing function should hit zero at the count of the most frequent word.
    val ngramWeights = new TextStatistics(text, stopwords).ngramCount.toSeq.map(
      _.map{
        case (seqwords, count) => (seqwords, count * (1.0/Math.max(txtstatGlobal(seqwords), 1)))
      }
    )

    def score(statsOther: TextStatistics) : Score = {
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

      new Score(
        ngramScores.foldLeft(0.0) {
          case (sum, listSeqScore) => sum + listSeqScore.map(_._2).sum
        },
        ngramScores.flatten.sortBy(_._2).takeRight(5)
      )
    }
  }

  class TextMatch[A](score: Score, val text: A) {
    val bestMatchingNGrams = score.matches.map{
      case (seqstr, f) => ("\"" + seqstr.mkString(" ") + "\"", f)
    }
  }

  def bestMatches[A<:WeightedStatistics, B<:TextStatistics](entries: Seq[A], texts: Seq[B], limit: Int) : Seq[(A, Double, TextMatch[B])] = {
    // TODO: This should calculate the prioqueue of the limit best scores, instead of calculating all entries x texts scores first
    texts.foldLeft(Seq.empty[(A, Score, B)]) {
      (seq, text) => {
        entries.foldLeft(seq) {
          (seq, entry) => { seq :+ ((entry, entry.score(text), text)) }
        }
      }
    }.sortBy(_._2.value). // sort by score
      takeRight(limit).
      map{
        case (entry, score, text) => (entry, score.value, new TextMatch[B](score, text))
      }
  }

  def bestMatchesGrouped[A<:WeightedStatistics, B<:TextStatistics](entries: Seq[A], texts: Seq[B], limit: Int) : Seq[(A, Double, Seq[TextMatch[B]])] = {
    val seqScoredMatches = texts.foldLeft(Seq.empty[(A, Score, B)]) {
      (seq, text) => {
        entries.foldLeft(seq) {
          (seq, entry) => { seq :+ ((entry, entry.score(text), text)) }
        }
      }
    }

    // TODO: Grouping the 'limit' best matches and _adding_ the scores is not a good idea
    // The quality of matches declines rapidly. Some bad matches may occur very often which
    // can in the end overshadow the good matches.
    seqScoredMatches.
      sortBy(_._2.value). // sort by score
      takeRight(limit).
      groupBy(_._1). // group by entry
      toSeq.
      map {         // turn into a Seq[entry, score, Seq[text]]
        case (entry, seq) => (
          entry,
          seq.map(_._2.value).sum,
          seq.map( t => new TextMatch[B](t._2, t._3) )
        )
      }.
      sortBy(_._2)
  }
}

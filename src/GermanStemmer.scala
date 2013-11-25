package text {
  object GermanStemmer {
    val vowels = Set('a', 'e', 'i', 'o', 'u', 'y', 'ä', 'ö', 'ü')
    val s_ending = Set('b', 'd', 'f', 'g', 'h', 'k', 'l', 'm', 'n', 'r', 't')
    val st_ending = Set('b', 'd', 'f', 'g', 'h', 'k', 'l', 'm', 'n', 't')

    def apply(input: String) : String = {
      // http://snowball.tartarus.org/algorithms/german/stemmer.html
      // test set: http://snowball.tartarus.org/algorithms/german/diffs.txt

      // Setup
      val word = new StringBuilder(input.toLowerCase.replaceAllLiterally("ß", "ss"))

      for( i <- 1 until word.length - 1
           if word(i)=='u' || word(i)=='y'
           if vowels(word(i-1)) && vowels(word(i+1))
      ) {
        if(word(i)=='u') {
          word(i) = 'U'
        } else {
          word(i) = 'Y'
        }
      }

      def indexWhere(word: StringBuilder, condition: Function[Char, Boolean], from: Int = 0): Int = {
        val idx = word.indexWhere(condition, from)
        if (idx == -1)
          word.length
        else
          idx
      }

      // Compute R1, R2
      val idx_vowel = indexWhere(word, vowels(_) )
      val idx_R1_internal = Math.min( indexWhere( word, !vowels(_), idx_vowel ) +1 , word.length )

      val idx_vowel_R1 = indexWhere( word, vowels(_), idx_R1_internal)
      val idx_R2 = Math.min( indexWhere( word, !vowels(_), idx_vowel_R1) + 1, word.length )

      val idx_R1 = Math.max(idx_R1_internal, 3)

      def validR1Suffix(suffix: String) : Boolean = {
        idx_R1 <= word.length - suffix.length
      }
      def validR2Suffix(suffix: String) : Boolean = {
        idx_R2 <= word.length - suffix.length
      }
      def deleteSuffix(suffix: String) {
        word.delete(word.length - suffix.length, word.length)
      }

      case class SuffixRule( suffixes: List[String], isValid: Function[String, Boolean], delete: Function[String, Unit] ) {
        def matches(word: StringBuilder) : Boolean = {
          suffixes.exists( suffix =>
            if(word.endsWith(suffix)) {
              if(isValid(suffix)) delete(suffix)
              true
            } else {
              false
            }
          )
        }
      }

      def process(rules: List[SuffixRule]) {
        // The suffixes in rules must be ordered from longest to shortest.
        // Processing stops at the first (longest) matching suffix.
        // It is deleted iff SuffixRule.isValid returns true
        rules.find( _.matches(word) )
      }

      // Step1
      process(List(
        SuffixRule(List("em", "ern", "er"), validR1Suffix, deleteSuffix),
        SuffixRule(List("e", "en", "es"), validR1Suffix, (suffix: String) => {
          deleteSuffix(suffix)
          if(word.endsWith("niss")) deleteSuffix("s")
        }),
        SuffixRule(List("s"), (suffix: String) => {
            validR1Suffix(suffix) && suffix.length < word.length && s_ending( word(word.length-suffix.length - 1))
          },
          deleteSuffix
        )
      ))

      // Step2
      process( List(
        SuffixRule(List("en", "er", "est"), validR1Suffix, deleteSuffix),
        SuffixRule(List("st"), (suffix: String) => {
            validR1Suffix(suffix) && suffix.length + 4 <= word.length && st_ending( word(word.length-suffix.length - 1))
          },
          deleteSuffix
        )
      ))

      // Step3
      process(List(
        SuffixRule(List("end", "ung"), validR2Suffix, (suffix: String) => {
          deleteSuffix(suffix)
          if(word.endsWith("ig") && !word.endsWith("eig") && validR2Suffix("ig"))
            deleteSuffix("ig")
        }),
        SuffixRule(List("ig", "ik", "isch"), (suffix: String) => {
          validR2Suffix(suffix) && word(word.length - suffix.length - 1)!='e'
        }, deleteSuffix),
        SuffixRule(List("lich", "heit"), validR2Suffix, (suffix: String) => {
          deleteSuffix(suffix)
          SuffixRule(List("er", "en"), validR1Suffix, deleteSuffix).matches(word)
        }),
        SuffixRule(List("keit"), validR2Suffix, (suffix: String) => {
          deleteSuffix(suffix)
          SuffixRule(List("lich", "ig"), validR2Suffix, deleteSuffix).matches(word)
        })
      ))

      word.map( c => c match {
        case 'ä' => 'a'
        case 'ö' => 'o'
        case 'ü' => 'u'
        case 'Y' => 'y'
        case 'U' => 'u'
        case _ => c
      }).mkString
    }
  }
}

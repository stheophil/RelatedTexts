package net.theophil

import play.api.libs.json._ // JSON library

package object relatedtexts {

  /**
   * One element of the list of [[Result]]s.
   * @param text the analyzable text that was matched against the analyzable text in the parent [[Result]] object
   * @param confidence the match confidence
   * @param matched the list of words that was found in both analyzable texts
   * @tparam T a type that extends the [[Analyzable]] trait, i.e., a class with a text and a list of keywords
   */
  case class ResultMatch[T <: Analyzable](text: T, confidence: Double, matched: Seq[String])

  /**
   * A list of analyzable texts that were matched against `text`.
   * @param text the analyzable text that was matched against the list of `matches`
   * @param confidence the highest confidence score of any match in `matches`
   * @param matches the list of [[ResultMatch]]es and their individual scores
   * @tparam S a type extending [[Analyzable]], i.e., a class with a text and a list of keywords
   * @tparam T a type extending [[Analyzable]]
   */
  case class Result[S <: Analyzable, T <: Analyzable](text: S, confidence: Double, matches: Seq[ResultMatch[T]])

  /**
   * Creates an implicit [[Writes]] object that converts a [[ResultMatch]][T] to JSON.
   * @tparam T
   * @return
   */
  implicit def matchWrites[T <: Analyzable] = new Writes[ResultMatch[T]] {
    def writes(m: ResultMatch[T]): JsValue = {
      Json.obj(
        "text" -> m.text.text,
        "keywords" -> m.text.keywords,
        "confidence" -> m.confidence,
        "matched" -> m.matched
      )
    }
  }

  /**
   * Creates an implicit [[Writes]] object that converts a [[Result]][S, T] to JSON.
   * @tparam S
   * @tparam T
   * @return
   */
  implicit def resultWrites[S <: Analyzable, T<:Analyzable] = new Writes[Result[S, T]] {
    def writes(result: Result[S, T]): JsValue = {
      Json.obj(
        "text" -> result.text.text,
        "keywords" -> result.text.keywords,
        "confidence" -> result.confidence,
        "matches" -> result.matches
      )
    }
  }

  /**
   * A simple default [[Analyzable]] implementation that can be used for matching texts that may also have a title,
   * and a url.
   * @param title The text's title. Also printed when the resulting matches are printed as JSON.
   * @param text The text that is matched.
   * @param keywords The list of keywords that will be matched. Keywords are always considered to have high relevance.
   * @param url The text's url. Also printed to JSON.
   */
  case class DefaultText(title: String, override val text: String, override val keywords: Seq[String], val url: String) extends Analyzable
  type DefaultResultMatch = ResultMatch[DefaultText]
  type DefaultResult = Result[DefaultText, DefaultText]
  val cTruncateTexts = 500

  /**
   * Creates an implicit [[Writes]] object that converts a [[ResultMatch]] of [[DefaultText]] to JSON. The `DefaultText.text`
   * will be truncated to 500 characters.
   * @return
   */
  implicit def matchDefaultWrites = new Writes[DefaultResultMatch] {
    def writes(m: DefaultResultMatch): JsValue = {
      Json.obj(
        "title" -> m.text.title,
        "text" -> m.text.text.substring(0, Math.min(m.text.text.length, cTruncateTexts)),
        "keywords" -> m.text.keywords,
        "url" -> m.text.url,
        "confidence" -> m.confidence,
        "matched" -> m.matched
      )
    }
  }

  /**
   * Creates an implicit [[Writes]] object that converts a [[Result]] of [[DefaultText]]s to JSON. The `DefaultText.text`
   * will be truncated to 500 characters.
   * @return
   */
  implicit def resultDefaultWrites = new Writes[DefaultResult] {
    def writes(result: DefaultResult): JsValue = {
      Json.obj(
        "title" -> result.text.title,
        "text" -> result.text.text.substring(0, Math.min(result.text.text.length, cTruncateTexts)),
        "keywords" -> result.text.keywords,
        "url" -> result.text.url,
        "confidence" -> result.confidence,
        "matches" -> result.matches
      )
    }
  }

  implicit def ngramCountWrites = new Writes[NGramCount] {
    def writes(c: NGramCount): JsValue = {
      Json.obj(
        "local" -> c.local,
        "global" -> c.global
      )
    }
  }

  /**
   * Creates an implicit [[Writes]] object that converts [[TextStatistics]] to JSON. 
   * @return
   */
  implicit def textstatWrites = new Writes[TextStatistics] {
    def writes(t: TextStatistics): JsValue = {
      Json.obj(
        "ngramsWithCount" -> t.ngramsWithCount,
        "keywords" -> t.keywords
      )
    }
  }
}

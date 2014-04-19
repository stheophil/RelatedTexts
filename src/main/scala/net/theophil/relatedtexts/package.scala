package net.theophil

import play.api.libs.json._ // JSON library
import play.api.libs.json.Reads._ // Custom validation helpers
import play.api.libs.functional.syntax._ // Combinator syntax

package object relatedtexts {
  type MapUrlToLength = collection.mutable.HashMap[String, Int] with collection.mutable.SynchronizedMap[String, Int]
  def newMapUrlToLength : MapUrlToLength = {
    new collection.mutable.HashMap[String, Int] with collection.mutable.SynchronizedMap[String, Int]
  }

  type AnalyzableItem = Item with Analyzable

  implicit def orderByMatchValue[T<:Analyzable] = Ordering.by[(T, TextMatch[AnalyzableItem]), Double](_._2.value)
  type SetBestMatches[T<:Analyzable] = collection.mutable.TreeSet[(T, TextMatch[AnalyzableItem])]

  case class DefaultText(title: String, override val text: String, override val keywords: Seq[String], val url: String) extends Analyzable
  case class ResultMatch(title: String, url: String, confidence: Double, matched: Seq[String])
  case class Result[T <: Analyzable](text: T, confidence: Double, articles: Seq[ResultMatch])

  implicit val matchWrites: Writes[ResultMatch] = (
    (JsPath \ "title").write[String] and
      (JsPath \ "url").write[String] and
      (JsPath \ "confidence").write[Double] and
      (JsPath \ "matched").write[Seq[String]]
    )(unlift(ResultMatch.unapply))

  implicit def resultWrites[T<:Analyzable] = new Writes[Result[T]] {
    def writes(result: Result[T]): JsValue = {
      Json.obj(
        "text" -> result.text.text,
        "confidence" -> result.confidence,
        "articles" -> result.articles
      )
    }
  }

  implicit def resultDefaultWrites = new Writes[Result[DefaultText]] {
    def writes(result: Result[DefaultText]): JsValue = {
      Json.obj(
        "title" -> result.text.title,
        "url" -> result.text.url,
        "confidence" -> result.confidence,
        "articles" -> result.articles
      )
    }
  }
}

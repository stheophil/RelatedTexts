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

  implicit val orderByMatchValue = Ordering.by[(Statement, TextMatch[AnalyzableItem]), Double](_._2.value)
  type SetBestMatches = collection.mutable.TreeSet[(Statement, TextMatch[AnalyzableItem])]

  case class Statement(title: String, override val text: String, override val keywords: Seq[String], val url: String) extends Analyzable
  case class ResultMatch(title: String, url: String, confidence: Double, matched: Seq[String])
  case class Result(title: String, url: String, confidence: Double, articles: Seq[ResultMatch])

  implicit val matchWrites: Writes[ResultMatch] = (
    (JsPath \ "title").write[String] and
      (JsPath \ "url").write[String] and
      (JsPath \ "confidence").write[Double] and
      (JsPath \ "matched").write[Seq[String]]
    )(unlift(ResultMatch.unapply))

  implicit val resultWrites: Writes[Result] = (
    (JsPath \ "title").write[String] and
      (JsPath \ "url").write[String] and
      (JsPath \ "confidence").write[Double] and
      (JsPath \ "articles").write[Seq[ResultMatch]]
    )(unlift(Result.unapply))
}

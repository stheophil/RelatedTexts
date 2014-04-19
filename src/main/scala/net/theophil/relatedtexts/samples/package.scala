package net.theophil.relatedtexts

import play.api.libs.json.{JsPath, Reads}
import play.api.libs.functional.syntax._ // Combinator syntax

package object samples {

  object JSON {
    // JSON format of /test_data/koalitionsvertrag.json
    case class InputText(id: Int, title: String, quote: String, tags: Seq[String])

    implicit val statementReads: Reads[InputText] = (
      (JsPath \ "id").read[Int] and
        (JsPath \ "title").read[String] and
        (JsPath \ "quote").read[String] and
        (JsPath \ "tags").read[Seq[String]]
      )(InputText.apply _)
  }
}

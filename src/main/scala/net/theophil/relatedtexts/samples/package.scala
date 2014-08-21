package net.theophil.relatedtexts

import java.io.{OutputStreamWriter, FileOutputStream}
import java.nio.charset.Charset

import play.api.libs.json._
import play.api.libs.functional.syntax._ // Combinator syntax

package object samples {
  // JSON format of /test_data/koalitionsvertrag.json
  case class InputText(id: Int, title: String, quote: String, tags: Seq[String])

  def removeMarkdownLink(text: String) : String = {
    val markdownLink = """\[([^\]]*)\]\(([^\)]*)\)""".r("text", "href")
    markdownLink.replaceAllIn( text, _ group "text" )
  }

  def loadSampleTexts : Seq[DefaultText] = {
    val jsonString = io.Source.fromInputStream(
      getClass.getResourceAsStream("/test_data/koalitionsvertrag.json"),
      "UTF-8"
    ).getLines().mkString("\n")

    val json: JsValue = Json.parse(jsonString)
    val seqInputs: JsResult[Seq[InputText]] = json.validate[Seq[InputText]]

    seqInputs.get.map( input => {
      DefaultText(
        removeMarkdownLink(input.title),
        removeMarkdownLink(input.title + " " + input.quote),
        input.tags,
        "http://www.wahlversprechen2013.de/item/" + input.id
      )
    })
  }

  def writeMatches(matches: String): Unit = {
    Console.println("Writing best matches to tmp/matches.json");
    val fos = new FileOutputStream("tmp/matches.json")
    val fileJSON = new OutputStreamWriter(fos, Charset.forName("UTF-8"))

    fileJSON.write(matches)

    fileJSON.close()
    fos.close()
  }

  implicit val statementReads: Reads[InputText] = (
    (JsPath \ "id").read[Int] and
      (JsPath \ "title").read[String] and
      (JsPath \ "quote").read[String] and
      (JsPath \ "tags").read[Seq[String]]
    )(InputText.apply _)
}

package net.theophil.relatedtexts.samples

import net.theophil.relatedtexts._
import java.io.{FileOutputStream, OutputStreamWriter}
import java.nio.charset.Charset

import play.api.libs.json._
import play.api.libs.functional.syntax._ // Combinator syntax

object StatExample {
  case class OutputText(id: Int, title: String, filtered_text: Seq[String], tags: Seq[String])
  implicit val outputTextWrites: Writes[OutputText] = (
    (JsPath \ "id").write[Int] and
      (JsPath \ "title").write[String] and
      (JsPath \ "filtered_text").write[Seq[String]] and
      (JsPath \ "tags").write[Seq[String]]
    )(unlift(OutputText.unapply))

  def main(args: Array[String]) {
    // Load sample content
    val jsonString = io.Source.fromInputStream(
      getClass.getResourceAsStream("/test_data/koalitionsvertrag.json"), 
      "UTF-8"
    ).getLines().mkString("\n")

    val json: JsValue = Json.parse(jsonString)
    val result: JsResult[Seq[InputText]] = json.validate[Seq[InputText]]

    // Load 1000 most frequent German words, calculate word stems
    val top1kWords = (for(line <- io.Source.fromInputStream(
      getClass.getResourceAsStream("/de/top1000de.txt"), 
      "UTF-16"
    ).getLines) yield GermanStemmer(line)).toSet

    result match {
      case s: JsSuccess[Seq[InputText]] => {

        // For each title/quote text in input jsonString
        // calculate list of n-grams after 1k most frequent words
        // have been removed and all words have been stemmed
        val inputWordSeq = s.get.map{ input =>
          val filtered_text = input.title + " " + input.quote
          val analytics = TextStatistics.withoutGlobalCount(filtered_text, Seq.empty[String], top1kWords)
          (input, analytics.ngramsWithCount.toSeq.map(_._1.mkString))
        }

        // Calculate number of occurrences for each remaining ngram
        // in the whole document
        val wordCount = inputWordSeq.map( _._2 ).flatten.
          foldLeft(Map.empty[String, Int])( (map, word) => {
            map + (word -> (map.getOrElse(word, 0) + 1))
        })

        // Output the id & title of the input text together with
        // the remaining word stems in descending order of relevance,
        // i.e. in ascending order of frequency
        val output = inputWordSeq.map{ case (input, wordSeq) => {
          val wordSeqFreq = wordSeq.
            map{ word => (word, wordCount(word)) }.
            sortBy(_._2).toSeq.map( t => t._1 + ":" + t._2)
          OutputText( input.id, input.title, wordSeqFreq, input.tags )
        }}

        val json = Json.toJson(output)
        
        val fos = new FileOutputStream("tmp/koalition_filtered.json")
        val file = new OutputStreamWriter(fos, Charset.forName("UTF-8"))
        file.write(json.toString())
        file.close()
        fos.close()
      }
      case e: JsError => println("Errors: " + JsError.toFlatJson(e).toString())
    }
  }
}

import org.jsoup.Jsoup
import scala.util.{Try, Success, Failure}

class Document(paragraphs: Seq[String]) {
  // Filters noise by removing all paragraphs below avg length
  // Documents should be filtered similarly using global statistics
  val avgLength = paragraphs.map( _.length ).sum / paragraphs.length
  val text = paragraphs.filter(_.length >= avgLength).mkString(" ")
}

class Item(val title : String, val link: String) {
  def paragraphs : Try[Document] = {
    try {
      import scala.collection.JavaConversions._
      val html = Jsoup.connect(link).get();
      Success( new Document(html.select("p").map(_.text())) )
    } catch {
      case e: Exception =>
        Failure(e)
    }
  }
}

class Feed(val url: String) {
    def items : Seq[Item] = {
      val feed = scala.xml.XML.load(url)
      feed.child.filter(_.label=="channel").flatMap(
        _.child.filter(_.label=="item").map( item => {
          val opttitle = item.child.find(_.label == "title").map(_.text)
          val optlink = item.child.find( _.label == "link").map(_.text)
          new Item(opttitle.getOrElse(""), optlink.get)
        })
      )
    }
}

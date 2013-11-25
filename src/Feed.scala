import org.jsoup.Jsoup
import scala.util.{Try, Success, Failure}

package feed {
  class ExtractedText(paragraphs: Seq[String]) {
    // Filters noise by removing all paragraphs below avg length
    // Documents should be filtered similarly using global statistics
    val avgLength = paragraphs.map( _.length ).sum / paragraphs.length
    val text = paragraphs.filter(_.length >= avgLength).mkString(" ")
  }

  class Item(val title : String, val link: String) {
    def extract : Try[ExtractedText] = {
      try {
        import scala.collection.JavaConversions._
        val html = Jsoup.connect(link).get();
        Success( new ExtractedText(html.select("p").map(_.text())) )
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

  object Feed {
    def newExtractedTexts(url: String, seen: (String) => Boolean) : Seq[(Item, ExtractedText)] = {
      try {
        val feed = new Feed(url)
        val itemdoc = feed.items.
          filter( item => !seen(item.link) ).
          map( item => (item, item.extract) ). // scrape feed on demand
          filter( _._2.isSuccess ).
          map( itemdoc => (itemdoc._1, itemdoc._2.get) )

        val avgLength = itemdoc.map( _._2.text.length ).sum / Math.max(itemdoc.length, 0)
        val shortArticle = (itemdoc: (Item, ExtractedText)) => itemdoc._2.text.length < avgLength/4

        itemdoc.filter( shortArticle ).foreach( itemdoc =>
          Console.println("[Warning] Document too short: " + itemdoc._1.link + " / " + itemdoc._2.text.length)
        )

        itemdoc.filter( !shortArticle(_) )
      } catch {
        case e: Exception => {
          Console.println("[Error] Failed to read feed: " + url + " : " + e.getMessage)
          Seq.empty[(Item, ExtractedText)]
        }
      }
    }
  }
}


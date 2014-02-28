import org.jsoup.Jsoup
import scala.util.{Try, Success, Failure}

package feed {

import java.util.Date

case class Item(title : String, link: String, text: String) {
    def withText : Try[Item] = {
      try {
        import scala.collection.JavaConversions._
        val html = Jsoup.connect(link).get();

        // Filters noise by removing all paragraphs below avg length
        // Documents should be filtered similarly using global statistics
        val paragraphs = html.select("p").map(_.text())
        val avgLength = paragraphs.map( _.length ).sum / paragraphs.length
        val text = paragraphs.filter(_.length >= avgLength).mkString(" ")
        Success(Item(title, link, text))
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
            Item(opttitle.getOrElse(""), optlink.get, "")
          })
        )
      }
  }

  object Feed {
    def newItemsWithText(url: String, lastSeenLength: (String) => Int, /* out */ newSeenLength: collection.mutable.SynchronizedMap[String, Int]) : Seq[Item] = {
      try {
        val feed = new Feed(url)
        val itemsAll = feed.items

        val itemsSeen = itemsAll.filter(item => lastSeenLength(item.link)>0 )
        // keep seen items in "seen" map
        itemsSeen.foreach( item => {
          newSeenLength.update(item.link, lastSeenLength(item.link))
        })

        val itemsNewWithText = itemsAll.filter( item => lastSeenLength(item.link)<=0 ).
          map( item => item.withText ). // scrape feed on demand
          filter( _.isSuccess ).
          map( _.get )

        // add new items to seen map
        itemsNewWithText.foreach( item => {
          newSeenLength.update(item.link, item.text.length)
        })

        val avgLength = (itemsNewWithText.map( _.text.length ).sum + itemsSeen.map(i => lastSeenLength(i.link)).sum) /
          Math.max(itemsNewWithText.length + itemsSeen.length, 0)

        val shortArticle = (item: Item) => item.text.length < avgLength/4
        itemsNewWithText.filter( shortArticle ).foreach( item =>
          Console.println("[Warning] Document too short: " + item.link + " / " + item.text.length)
        )

        itemsNewWithText.filter( !shortArticle(_) )
      } catch {
        case e: Exception => {
          Console.println("[Error] Failed to read feed: " + url + " : " + e.getMessage)
          Seq.empty[Item]
        }
      }
    }
  }
}


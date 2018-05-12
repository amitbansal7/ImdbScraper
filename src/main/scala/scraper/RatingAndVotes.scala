package scraper

import org.jsoup.Jsoup
import org.jsoup.nodes.Document

object RatingAndVotes {


  def main(args: Array[String]): Unit = {

    val id = "tt0111161";
    val proxies = Proxy.getProxies
    print(s"Fetching rating and number of votes for id $id ")
    println(getRatingAndVotes(id, proxies(0)))
  }

  def getRatingAndVotes(id: String, proxy: (String, Int)) = {

    val url = "http://www.imdb.com/title/" + id + "/"
    val doc = JsoupDoc.getJsoupDoc(url, proxy)
    val div = doc.select("div.imdbRating")

    val rating = div.select("span[itemprop=ratingValue]").text.toDouble
    val votes = div.select("span[itemprop=ratingCount]").text.replace(",", "").toDouble

    (rating, votes)
  }
}

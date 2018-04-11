package scraper

import org.jsoup.Jsoup
import org.jsoup.nodes.{Document, Element}
import org.jsoup.select.Elements

import scala.annotation.tailrec

object ReviewScraper {

  class Review(title: String,
               text: String,
               user: String,
               name: String,
               date: String,
               score: String,
               spoiler: Boolean,
               votes: String,
               foundHelpful: String
              ) {
    override def toString: String = {
      title + " | " +
        name + " | " +
        user + " | " +
        date + " | " +
        spoiler + " | " +
        score + " | " +
        votes + " | " +
        foundHelpful + " | " +
        text
    }
  }

  def getProxies(): Vector[(String, Int)] = {

    def getAllTds(element: Element): Vector[String] = {
      def helper(acc: Vector[String], elements: Elements, idx: Int): Vector[String] = {
        if (idx >= elements.size()) acc
        else helper(acc :+ elements.get(idx).text(), elements, idx + 1)
      }

      helper(Vector[String](), element.select("td"), 0)
    }

    def loopOver(elements: Elements, idx: Int, acc: Vector[(String, Int)]): Vector[(String, Int)] = {
      if (idx <= 1) acc
      else {
        val allTds = getAllTds(elements.get(idx))
        if (allTds.size == 8 && !allTds(4).equals("transparent") && !allTds(6).equals("no"))
          loopOver(elements, idx - 1, (allTds(0), allTds(1).toInt) +: acc)
        else loopOver(elements, idx - 1, acc)
      }
    }

    def getJsoupDoc() =
      Jsoup
        .connect("https://www.us-proxy.org/")
        .userAgent("Mozilla")
        .get()

    val elements = getJsoupDoc.attr("id", "proxylisttable").select("tr")
    loopOver(elements, elements.size() - 1, Vector[(String, Int)]())
  }

  def getAllreviews(id: String, limit: Int, prox: (String, Int)): List[Review] = {

    def reviewToReviewModel(review: Element): List[Review] = {

      def scoreOrMinus1(elements: Elements): String = {
        try {
          elements.get(1).text()
        } catch {
          case e: Exception => "-1"
        }
      }

      List(new Review(
        review.select("div[class=title]").text(),
        review.select("div[class=text show-more__control]").text(),
        review.select("div[class=display-name-date]").select("a[href]").attr("href").split("/")(2),
        review.select("div[class=display-name-date]").select("span").get(0).select("a").text(),
        review.select("div[class=display-name-date]").select("span").get(1).text(),
        scoreOrMinus1(review.select("div[class=ipl-ratings-bar]").select("span")),
        !review.select("span[class=spoiler-warning]").isEmpty,
        review.select("div[class=actions text-muted]").text.split("\n")(0).split(" ")(3).replace(",", ""),
        review.select("div[class=actions text-muted]").text.split("\n")(0).split(" ")(0).replace(",", ""),
      ))
    }

    @tailrec
    def scrapeReviewContainer(elements: Elements, idx: Int, acc: List[Review], count: Int, limit: Int): List[Review] = {
      if (idx >= elements.size() || count >= limit) acc
      else scrapeReviewContainer(elements, idx + 1, acc ::: reviewToReviewModel(elements.get(idx)), count + 1, limit)
    }

    def getJsoupDoc(url: String): Document =
      Jsoup
        .connect(url)
        .userAgent("Mozilla/5.0 (Windows NT 6.1; Win64; x64; rv:25.0) Gecko/20100101 Firefox/25.0")
        .referrer("http://www.google.com")
        .proxy(prox._1, prox._2)
        .get()

    def findWithKey(str: String, id: String, count: Int, limit: Int): List[Review] = {
      if (str.equals("") || count >= limit) Nil
      else {
        val doc = getJsoupDoc("http://www.imdb.com/title/" + id + "/reviews/_ajax?ref_=undefined&paginationKey=" + str)
        val res = scrapeReviewContainer(doc.select("div[class=review-container]"), 0, Nil, count, limit)
        res ::: findWithKey(doc.select("div[class=load-more-data]").attr("data-key"), id, count + res.size, limit)
      }
    }

    val doc = getJsoupDoc("http://www.imdb.com/title/" + id + "/reviews/?sort=submissionDate&dir=desc")
    val res = scrapeReviewContainer(doc.select("div[class=review-container]"), 0, Nil, 0, limit)
    res ::: findWithKey(doc.select("div[class=load-more-data]").attr("data-key"), id, res.size, limit)
  }


  def main(args: Array[String]): Unit = {

    //Movie id
    val id = "tt0371746";

    //Maximum number of reviews
    val limit = 200

    println(s"Fetching reviews for $id")
    val start = System.currentTimeMillis()
    val proxies = getProxies
    val res = getAllreviews(id, limit, proxies(0))
    val end = System.currentTimeMillis()

    res.foreach {
      case r: Review => println(r.toString)
    }

    println("Time " + (end - start))
    println("Size " + res.size)
  }
}

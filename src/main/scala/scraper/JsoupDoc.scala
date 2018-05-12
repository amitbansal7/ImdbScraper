package scraper

import org.jsoup.Jsoup
import org.jsoup.nodes.Document

object JsoupDoc {

  def getJsoupDoc(url: String, proxy: (String, Int)): Document =
    Jsoup
      .connect(url)
      .userAgent("Mozilla/5.0 (Windows NT 6.1; Win64; x64; rv:25.0) Gecko/20100101 Firefox/25.0")
      .referrer("http://www.google.com")
      .proxy(proxy._1, proxy._2)
      .get()
}

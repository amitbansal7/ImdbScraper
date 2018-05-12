package scraper

import org.jsoup.Jsoup
import org.jsoup.nodes.Element
import org.jsoup.select.Elements

object Proxy {
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
}

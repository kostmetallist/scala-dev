import java.io.PrintStream
import java.net.{ InetSocketAddress, Proxy }

import scala.collection.immutable.SortedMap

import net.ruippeixotog.scalascraper.browser.{ HtmlUnitBrowser, JsoupBrowser }
import net.ruippeixotog.scalascraper.dsl.DSL.Extract._
import net.ruippeixotog.scalascraper.dsl.DSL._
import net.ruippeixotog.scalascraper.model.Element
import net.ruippeixotog.scalascraper.scraper.HtmlValidator
import net.ruippeixotog.scalascraper.util.EitherRightBias._


object Extractor extends App {

  val browser = HtmlUnitBrowser.typed()
  val doc = browser.get("http://example.com")


  // can extract from https://www.exchange-rates.org/

  val moreInfoLink = doc >> pElement("a")
  moreInfoLink.underlying.click()

  doc >> text("h1").map("== " + _ + "==") |> println
  doc >> texts("p") foreach println
}

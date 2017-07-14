package top.wagzhi.ssoup.parser

import org.scalatest.{Matchers, Outcome}
import Selecting._
import org.jsoup.Jsoup
import org.jsoup.nodes.Document
import top.wagzhi.ssoup.ScalaConvert._
import scala.io.Source
import scala.collection.JavaConverters._
/**
  * Created by paul on 2017/7/14.
  */
class ScalaConvertTest extends org.scalatest.fixture.FlatSpec with Matchers{
  //*[@id="view-hd"]/h1/a/span

  case class FixtureParam(html:Seq[Document])


  override protected def withFixture(test: OneArgTest): Outcome = {
    try{
      val cl = getClass.getClassLoader
      val inputs = Seq(cl.getResource("test.html"),cl.getResource("index.html"))

      val htmls = inputs.map{
        input=>
          Source.fromFile(input.toURI).mkString
      }.map(Jsoup.parse)
      withFixture(test.toNoArgTest(FixtureParam(htmls)))
    }finally {
    }

  }
  "ScalaConvert test" should "parse thread page" in{
    f=>
      val doc = f.html.head
      val e= doc.findByXPath("//*[@id='view-hd']/h1/a/span")
      e should have length 1
  }
  "ScalaConvert test" should "parse index page" in{
    f=>
      val doc = f.html(1)
      //*[@id=\"wrap\"]/article/section[1]/div/div/div[1]"
      val es= doc.findByXPath("//*[@id='wrap']/article/section[1]/div/div/div[1]")
      es should have length 1
      es(0).getFullName() shouldBe "<div class='cnt-main'>"

      val me = es(0)
      val links = me.links()
      links should (contain allOf(
            "http://ent.19lou.com/",
            "http://www.19lou.com/forum-269-thread-5171499957751417-1-1.html")
            and have length 18)

  }


}

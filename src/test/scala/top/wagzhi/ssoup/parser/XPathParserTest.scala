package top.wagzhi.ssoup.parser

import org.scalatest.{Matchers, Outcome}
import Selecting._
/**
  * Created by paul on 2017/7/13.
  */
class XPathParserTest extends org.scalatest.fixture.FlatSpec with Matchers{
  case class FixtureParam(xpaths: Seq[String])
  override protected def withFixture(test: OneArgTest): Outcome = {
    val xpaths = Seq("//book/java","book/java","/all//book/java","/div/p[@class='es'][@id]")
    try {
      withFixture(test.toNoArgTest(FixtureParam(xpaths)))
    } finally {
    }
  }
  "xpathparser" should "parse " in{
    f=>
      val results = f.xpaths.map(XPathParser.parse)
      val r1 = results(0)
      val r2 = results(1)
      val r3 = results(2)
      val r4 = results(3)
      r1.length shouldBe 2
      r1(0) shouldBe XPathQueue(SelectingAllChild,Predicate("book"))
      r1(1) shouldBe XPathQueue(SelectingRoot,Predicate("java"))


      r2(0) shouldBe XPathQueue(SelectingNone,Predicate("book"))
      r2(1) shouldBe XPathQueue(SelectingRoot,Predicate("java"))

      r3(0) shouldBe XPathQueue(SelectingRoot,Predicate("all"))
      r3(1) shouldBe XPathQueue(SelectingAllChild,Predicate("book"))
      r3(2) shouldBe XPathQueue(SelectingRoot,Predicate("java"))

      r4(1).predicate.queries.head shouldBe AttributeValueQuery("class","es")
      r4(1).predicate.queries.last shouldBe AttributeQuery("id")

  }



}

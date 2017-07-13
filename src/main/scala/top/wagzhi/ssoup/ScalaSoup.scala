package top.wagzhi.ssoup

import org.jsoup.Jsoup
import org.jsoup.nodes.Element

/**
  * Created by paul on 2017/7/13.
  */
object ScalaSoup {
  def parse(html:String,baseUri:String="") = {
    Jsoup.parse(html,baseUri)
  }

}

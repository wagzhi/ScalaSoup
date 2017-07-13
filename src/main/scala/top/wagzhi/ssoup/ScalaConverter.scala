package top.wagzhi.ssoup

import org.jsoup.nodes.Element
import org.jsoup.select.Collector
import top.wagzhi.ssoup.parser.{Selecting, XPathParser, XPathQueue}
import top.wagzhi.ssoup.select.{CombinedEvaluator, PredicateEvaluator}

import scala.collection.JavaConversions._
/**
  * Created by paul on 2017/7/13.
  */
object ScalaConvert {
  implicit class SElement(val e: Element){
      //遍历每个element，并执行转换函数后通过Seq返回
      def map[T](func: Element => Option[T],deep:Int =0): Seq[T] = {
        val seq = func(e).map(Seq(_)).getOrElse(Seq.empty[T])

        if (e.children().isEmpty) {
          seq
        } else {
          e.children().toIterable.foldLeft(seq) {
            (s: Seq[T], ce: Element) =>
              val s2: Seq[T] = ce.map(func,deep+1)
              s ++: s2
          }
        }
      }

      //抽取出Element中所有链接
      def links(protocol:String="http://"):Seq[String] ={
        e.map{
          el=>
            if(el.nodeName()=="a"){
              Some(el)
            }else{
              None
            }
        }.map{
          e=>
            val link = e.attr("href")

            if (link.startsWith("//")){ //泛协议
              protocol + link.substring(2)
            }else if (link.startsWith("http://") || link.startsWith("https")){ //完整路径
              link
            }else if (link.startsWith("/")) {//绝对路径
              e.baseUri()+link
            }else if(link.startsWith("./")){ //相对路径
              e.baseUri().lastIndexOf("/") +link
            }
            else{ //未知路径，过滤掉
              ""
            }
        }
          .filter(_.length>0).toList
      }


      def select(xpath:String) ={
        val qs = XPathParser.parse(xpath)
        qs.foldLeft(Seq[Element](e)){
          (es,xq)=>
            es.flatMap {
              ine =>
                query(ine, xq)
            }
        }
      }
      def query(root: Element,xq:XPathQueue): Seq[Element] ={
        val evaluator = new PredicateEvaluator(xq.predicate)
        xq.selecting match {
          case Selecting.SelectingAllChild =>{
            Collector.collect(evaluator,e).toList.toSeq
          }
          case Selecting.SelectingRoot =>{
            root.children().toSeq.filter{
              e=>
                evaluator.matches(root,e)
            }
          }case Selecting.SelectingNone =>{
            if(evaluator.matches(root.parent(),root)){
              Seq(root)
            }else{
              Seq()
            }
          }case _=>{
            Seq[Element]()
          }
        }
      }

      /**
        * 查找符合条件的Element列表
        * @param tagName  如"div"
        * @param idStarting 如"uid_",可以匹配"uid_132456",完整匹配也可以
        * @param classNames 逐个匹配Element的class，如果Seq参数空，则不匹配这一项
        * @return
        */
      def findElements(tagName:String,idStarting:String = "",classNames:Seq[String] = Seq[String]()): Seq[Element] ={
        var evaluator = new CombinedEvaluator(tagName,idStarting,classNames)
        Collector.collect(evaluator,e).toList.toSeq
      }

      def findElements(tagName:String,classNames:Seq[String]) : Seq[Element]  =
        findElements(tagName,idStarting = "",classNames = classNames)

      def findElement(tagName:String,idStarting:String = "",classNames:Seq[String] = Seq[String]()):Option[Element] =
        findElements(tagName,idStarting,classNames).headOption

      def findElement(tagName:String,classNames:Seq[String]):Option[Element] = findElements(tagName,classNames).headOption


      def getAllElementsByTags(tagNames:String*)= {
        val tags = tagNames.map(_.toLowerCase)
        e.getAllElements.toIndexedSeq.filter{
          el=>
            tags.contains(el.tagName().toLowerCase)
        }
      }
      /**
        * 去除指定名称的元素，比如"script"，"link"等。
        * @param tagNames
        * @return
        */
      def clean(tagNames:Seq[String]):Element ={
        val tn = tagNames.map(_.toLowerCase)
        this.map[Element]{
          e=>
            if(tn.contains(e.tagName().toLowerCase)){
              e.remove()
            }
            None
        }
        this.e
      }


  }
}

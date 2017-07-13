package top.wagzhi.ssoup.parser

import top.wagzhi.ssoup.IllegalXpathException

import scala.collection.JavaConversions._
/**
  * Created by paul on 2017/7/13.
  */
object XPathParser {
  val COMBINATOR_ROOT = "/"
  val COMBINATOR_ALL = "//"
  val COMBINATORS = Seq(COMBINATOR_ROOT,COMBINATOR_ALL)
  import Selecting._
  def parse(xpath:String)={
    xpath.split(COMBINATOR_ALL).toList.foldLeft(List[(Int,String)]()){
      (l,path)=>
        l :+ (l.length,path)
    }.flatMap{
      case(idx:Int,path:String)=>
        if(idx ==0 && path.isEmpty){
          Seq()
        }else{
          path.split(COMBINATOR_ROOT).toList.foldLeft(List[(Int,String)]()){
            (l,path)=>
              l :+ (l.length,path)
          }.filter(_._2.length>0).map{
            case (idx2:Int,path:String)=>{
               if(idx >0 && idx2==0){
                 XPathQueue(SelectingAllChild,Predicate(path))
               }else if(idx ==0 && idx2==0){
                 XPathQueue(SelectingNone,Predicate(path))
               }else{
                 XPathQueue(SelectingRoot,Predicate(path))
               }
            }
          }
        }
    }
  }
}

case class XPathQueue(selecting:Selecting.Selecting,predicate:Predicate)

trait Query{

}
case class AttributeQuery(attribute:String) extends Query
case class AttributeValueQuery(attribute:String,value:String) extends Query
case class IndexQuery(index:String) extends Query

object Predicate{
  def apply(predicate: String): Predicate ={
    val sp =predicate.split('[')
    val tagName = sp.head
    val queries = sp.slice(1,sp.length).map(_.split(']').head).map{
      q=>
       if(q.startsWith("@")){
         val qs = q.substring(1).split("=")
         if(qs.length==1){
           AttributeQuery(qs.head)
         }else if(qs.length==2){
           val key = qs(0)
           val value = qs(1).filter(_!=''')
           AttributeValueQuery(key,value)
         }else{
           throw new IllegalXpathException()
         }
       }else{
         IndexQuery(q)
       }
    }
    Predicate(tagName,queries)
  }
}

case class Predicate(val tagName:String,val queries:Seq[Query]){

}

object Selecting extends Enumeration{
  type Selecting = Selecting.Value
  val SelectingAllChild = Value("//")
  val SelectingRoot = Value("/")
  val SelectingNone = Value("")
}
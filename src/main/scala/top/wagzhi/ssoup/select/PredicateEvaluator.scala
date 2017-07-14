package top.wagzhi.ssoup.select

import org.jsoup.nodes.Element
import org.jsoup.select.Evaluator
import top.wagzhi.ssoup.parser.{AttributeQuery, AttributeValueQuery, IndexQuery, Predicate}

import scala.collection.JavaConversions._
import top.wagzhi.ssoup.ScalaConvert._

/**
  * Created by paul on 2017/7/13.
  */
class PredicateEvaluator(predicate:Predicate) extends Evaluator{
  override def matches(root: Element, element: Element): Boolean = {
    if(predicate.tagName!="*" && (!predicate.tagName.equalsIgnoreCase(element.tagName()))){
      false
    }else{
      !predicate.queries.map{
        q=>
          q match {
            case AttributeQuery(attribute)=>{
              element.attributes().toSeq.filter{
                a=>
                  a.getKey.equalsIgnoreCase(attribute)
              }.nonEmpty
            }
            case AttributeValueQuery(key,value)=>{
              element.attr(key).equals(value)
            }
            case IndexQuery(index)=>{
              val tags = root.children.toList.filter(_.tagName().equals(element.tagName()))
              try{
                val idx = index.toInt-1
                val theElement = tags(idx)
                theElement.equals(element)
              }catch{
                case _:Throwable=>
                  val es = index.split("-")
                  if(es.head=="last()"&&es.length==1){
                    tags.lastOption.map(_.equals(element)).getOrElse(false)
                  }else if(es.head=="last()"&& es.length==2){
                    try{
                      tags(tags.length-es(1).toInt-1).equals(element)
                    }catch{
                      case _:Throwable=> false
                    }
                  }else{
                    false
                  }
              }
            }
            case _=>{
              false
            }
          }


      }.contains(false)
    }
  }
}

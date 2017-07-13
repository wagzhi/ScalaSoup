package top.wagzhi.ssoup.select

import org.jsoup.nodes.Element
import org.jsoup.select.Evaluator

/**
  * Created by paul on 2017/6/21.
  *
  * combine tag evaluator and attribute value evaluator
  */
class CombinedEvaluator(tagName:String, idStarting:String, classNames:Seq[String]) extends Evaluator{

  override def matches(root: Element, element: Element): Boolean = {
    if(tagName.equalsIgnoreCase(element.tagName())){
      if(idStarting.isEmpty || new Evaluator.AttributeWithValueStarting("id",idStarting).matches(root,element)){
        !classNames.map{
          className=>
            element.hasClass(className)
        }.contains(false)
      }else{
        false
      }
    }else{
      false
    }
  }
}

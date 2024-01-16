package utils

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axilite.{AxiLite4, AxiLite4SlaveFactory}

import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps

object BundleParser {

  def apply(bundle: Bundle) = {
    val splitedElems = ArrayBuffer[(String, Int)]()
    for (elem <- bundle.elements) {
      val name = bundle.getName + "_" + elem._1
      val data = elem._2

      data match {
        case m: MultiData =>
          for (subElem <- m.elements)
            splitedElems += ((name + "_" + subElem._1, subElem._2.getBitsWidth))
        case s => splitedElems += ((name, s.getBitsWidth))
      }
    }
    splitedElems.toArray
  }
}

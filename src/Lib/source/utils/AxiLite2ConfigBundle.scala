package utils

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axilite.{AxiLite4, AxiLite4SlaveFactory}

import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps

object AxiLite2ConfigBundle {
  def apply(ctrl: AxiLite4SlaveFactory, bundle: Bundle, baseAddr: BigInt) = {
    var addr = baseAddr
    val inc = ctrl.busDataWidth / 8

    val splitedElems = ArrayBuffer[(String, Data)]()
    for (elem <- bundle.elements) {
      val name = elem._1
      val data = elem._2

      data match {
        case m: MultiData =>
          if (m.getBitsWidth > 32) {
            for (subElem <- m.elements)
              splitedElems += ((name + "_" + subElem._1, subElem._2))
          }
          else
            splitedElems += ((name, m))
        case s => splitedElems += ((name, s))
      }
    }

    val ret = new Area {
      for (elem <- splitedElems) {
        val reg = Bits(elem._2.getBitsWidth bits) setAsReg() init 0
        val regDly = RegNext(reg, init = B(0))

        reg.setName(bundle.name + elem._1 + "_reg")
        regDly.setName(bundle.name + elem._1 + "_regDly")
        regDly.addAttribute("MAX_FANOUT", "100")

        ctrl.write(reg, addr, 0, s"Write ${bundle.name}'s ${elem._1}")
        elem._2.assignFromBits(regDly)
        addr += inc
      }
    }
    ret
  }
}

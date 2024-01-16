package utils

import fifos.StreamFifoHighPerf
import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axilite.{AxiLite4, AxiLite4SlaveFactory}

import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps

object AxiLite42Stream {
  def apply(ctrl: AxiLite4SlaveFactory, bus: AxiLite4, addr: BigInt) = {
    val ret = Flow(Bits(ctrl.busDataWidth bits))
    ret.valid.clear()
    ctrl.onWrite(addr)(ret.valid.set())
    ret.payload := bus.w.payload.data
    ret
  }
}

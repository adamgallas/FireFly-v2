package utils

import spinal.core._
import spinal.lib._
import spinal.core.sim._
import spinal.lib.bus.amba4.axilite.{AxiLite4, AxiLite4SpecRenamer}

import scala.language.postfixOps

case class AxiLite4ToStream(addressWidth: Int, dataWidth: Int) extends Component {
  val io = new Bundle {
    val S_AXI_LITE = slave(AxiLite4(addressWidth, dataWidth))
    val M_AXIS = master(Stream(Bits(dataWidth bits)))
  }
  noIoPrefix()
  AxiLite4SpecRenamer(io.S_AXI_LITE)
  utils.AxiStreamSpecRenamer(io.M_AXIS)

  io.S_AXI_LITE.ar.freeRun()
  io.S_AXI_LITE.r.setIdle()

  val join = StreamJoin(io.S_AXI_LITE.aw, io.S_AXI_LITE.w)
  io.M_AXIS << join.translateWith(join._2.data).m2sPipe()

  val bvalid = Bool() setAsReg() init false
  bvalid clearWhen io.S_AXI_LITE.b.ready setWhen join.fire
  io.S_AXI_LITE.b.valid := bvalid
  io.S_AXI_LITE.b.setOKAY()
}

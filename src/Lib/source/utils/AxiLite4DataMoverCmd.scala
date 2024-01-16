package utils

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axilite.{AxiLite4, AxiLite4SpecRenamer}

import scala.language.postfixOps

case class AxiLite4DataMoverCmd(addressWidth: Int) extends Component {
  val io = new Bundle {
    val S_AXI_LITE = slave(AxiLite4(addressWidth, 64))
    val cmd = master(Stream(Bits(72 bits)))
  }
  noIoPrefix()
  AxiLite4SpecRenamer(io.S_AXI_LITE)
  utils.AxiStreamSpecRenamer(io.cmd)

  io.S_AXI_LITE.r.arbitrationFrom(io.S_AXI_LITE.ar)
  io.S_AXI_LITE.r.data := io.cmd.isStall.asBits.resize(64 bits)
  io.S_AXI_LITE.r.setOKAY()

  val join = StreamJoin(io.S_AXI_LITE.aw, io.S_AXI_LITE.w)
  val concat = U"00000000" ## join._2.data
  io.cmd << join.translateWith(concat).m2sPipe()

  val bvalid = Bool() setAsReg() init false
  bvalid clearWhen io.S_AXI_LITE.b.ready setWhen join.fire
  io.S_AXI_LITE.b.valid := bvalid
  io.S_AXI_LITE.b.setOKAY()
}

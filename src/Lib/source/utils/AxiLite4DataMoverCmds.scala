package utils

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axilite.{AxiLite4, AxiLite4SpecRenamer}

import scala.language.postfixOps

case class AxiLite4DataMoverCmds(
                                  portNumber: Int
                                ) extends Component {
  val io = new Bundle {
    val S_AXI_LITE = slave(AxiLite4(32, 64))
    val cmd = Vec(master(Stream(Bits(72 bits))), portNumber)
  }
  noIoPrefix()
  AxiLite4SpecRenamer(io.S_AXI_LITE)
  io.cmd.foreach(utils.AxiStreamSpecRenamer(_))
  val portWidth = log2Up(portNumber)

  val raddrLow = io.S_AXI_LITE.ar.addr.drop(4).take(portWidth).asUInt
  io.S_AXI_LITE.r.arbitrationFrom(io.S_AXI_LITE.ar)
  io.S_AXI_LITE.r.data := io.cmd(raddrLow).isStall.asBits.resize(64 bits)
  io.S_AXI_LITE.r.setOKAY()

  val join = StreamJoin(io.S_AXI_LITE.aw, io.S_AXI_LITE.w)
  val concat = U"00000000" ## join._2.data

  val waddrLow = join._1.addr.drop(4).take(portWidth).asUInt
  val ports = StreamDemux(join.translateWith(concat), waddrLow, portNumber)
  (io.cmd, ports).zipped.foreach(_ << _.m2sPipe())

  val bvalid = Bool() setAsReg() init false
  bvalid clearWhen io.S_AXI_LITE.b.ready setWhen join.fire
  io.S_AXI_LITE.b.valid := bvalid
  io.S_AXI_LITE.b.setOKAY()
}

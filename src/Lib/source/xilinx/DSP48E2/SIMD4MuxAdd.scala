package xilinx.DSP48E2

import spinal.core._

import scala.language.postfixOps

case class SIMD4MuxAdd(useModel: Boolean = false) extends Component {
  val io = new Bundle {
    val sel = in Vec(Bool(), 2)
    val din = in Vec(Vec(SInt(8 bits), 4), 2)
    val dout = out Vec(SInt(12 bits), 4)
    val CEs = in Vec(Bool(), 2)
  }
  noIoPrefix()

  val muxAdd = FOUR12MuxAdd(useModel)
  muxAdd.io.CEs := io.CEs
  muxAdd.io.AB := Vec(io.din(0).map(_.resize(12 bits))).asBits
  muxAdd.io.C := Vec(io.din(1).map(_.resize(12 bits))).asBits
  muxAdd.io.ABSel := io.sel(0)
  muxAdd.io.CSel := io.sel(1)
  io.dout := Vec(muxAdd.io.P.subdivideIn(12 bits).map(_.asSInt))
}
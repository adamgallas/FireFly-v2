package xilinx.DSP48E2

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

case class SIMD2INT8_24ACC(useModel: Boolean = false) extends Component {
  val io = new Bundle {
    val curr = in Vec(SInt(8 bits), 2)
    val prev = in Vec(SInt(18 bits), 2)
    val fetch = in Bool()
    val CEs = in Vec(Bool(), 2)
    val outputs = out Vec(SInt(18 bits), 2)
  }
  noIoPrefix()
  val dsp = TWO24AddWithFastFetch(useModel)
  dsp.io.AB := io.curr(1).resize(24 bits) ## io.curr(0).resize(24 bits)
  dsp.io.C := io.prev(1).resize(24 bits) ## io.prev(0).resize(24 bits)
  dsp.io.CEs := io.CEs
  dsp.io.FastFetch := io.fetch
  io.outputs(0) := dsp.io.P.take(24).asSInt.resize(18 bits)
  io.outputs(1) := dsp.io.P.drop(24).asSInt.resize(18 bits)
}

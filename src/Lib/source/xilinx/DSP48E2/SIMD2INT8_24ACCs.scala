package xilinx.DSP48E2

import spinal.core._

import scala.language.postfixOps

case class SIMD2INT8_24ACCs(rows: Int, useModel: Boolean = false) extends Component {
  require(rows % 2 == 0)
  val io = new Bundle {
    val curr = in Vec(SInt(8 bits), rows)
    val prev = in Vec(SInt(18 bits), rows)
    val fetch = in Bool()
    val CEs = in Vec(Bool(), 2)
    val outputs = out Vec(SInt(18 bits), rows)
  }
  noIoPrefix()
  val elem = Array.fill(rows / 2)(xilinx.DSP48E2.SIMD2INT8_24ACC(useModel))
  val currGroup = io.curr.grouped(2).map(Vec(_)).toArray
  val prevGroup = io.prev.grouped(2).map(Vec(_)).toArray
  elem.foreach(_.io.fetch := io.fetch)
  elem.foreach(_.io.CEs := io.CEs)
  (elem, currGroup).zipped.foreach(_.io.curr := _)
  (elem, prevGroup).zipped.foreach(_.io.prev := _)
  io.outputs.assignFromBits(Vec(elem.map(_.io.outputs)).asBits)
}
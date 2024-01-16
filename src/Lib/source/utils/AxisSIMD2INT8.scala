package utils

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

case class AxisSIMD2INT8() extends Component {
  val io = new Bundle {
    val S_AXIS = slave(Stream(Bits(32 bits)))
    val M_AXIS = master(Stream(Bits(32 bits)))
  }
  noIoPrefix()
  AxiStreamSpecRenamer(io.S_AXIS)
  AxiStreamSpecRenamer(io.M_AXIS)

  def op(din: Bits, cond: Vec[Bool]): Bits = {
    val mult = xilinx.DSP48E2.SIMD2INT8_16MULT()
    mult.io.a := din(7 downto 0).asSInt
    mult.io.b := din(15 downto 8).asSInt
    mult.io.c := din(23 downto 16).asSInt
    mult.io.CEs := cond
    val ret = Bits(32 bits)
    ret := mult.io.ac.asBits ## mult.io.ab.asBits
    ret
  }

  StreamMultiStageLogic(io.S_AXIS, io.M_AXIS, 4, op)
}

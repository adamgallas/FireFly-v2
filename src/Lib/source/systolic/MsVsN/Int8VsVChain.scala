package systolic.MsVsN

import spinal.core._
import spinal.lib._
import systolic._
import xilinx.DSP48E2._
import scala.language.postfixOps

case class Int8VsVChain(length: Int, needInit: Boolean = false)
  extends AdderChain(Vec(Bits(1 bits), 2), Vec(Vec(Bits(8 bits), 4), 2), Vec(Bits(12 bits), 4), length, needInit) {

  val dsp48s = for (i <- 0 until length) yield {
    DSP48E2Build().genDynamicALUDSP48E2(
      pipe = (1, 1, 1, 0, 0),
      enable = (inputs.value(i)(1).asBool, False, Bool(i != 0), inputs.value(i)(0).asBool),
      mode = "4"
    )
  }
  for (i <- 1 until length)
    dsp48s(i).CASCDATAIN.P := dsp48s(i - 1).CASCDATAOUT.P

  val union = Vec(
    Vec(inputs.linked.map(s => Vec(s(0).map(_.asSInt.resize(12 bits))).asBits)),
    Vec(inputs.linked.map(s => Vec(s(1).map(_.asSInt.resize(12 bits))).asBits))
  )
  (dsp48s, union(1)).zipped.foreach(_.DATAIN.C := _)
  (dsp48s, union(0)).zipped.foreach(_.DATAIN.A := _.drop(18))
  (dsp48s, union(0)).zipped.foreach(_.DATAIN.B := _.take(18))
  io.outputs.fragment := Vec(dsp48s.last.DATAOUT.P.subdivideIn(12 bits).map(_.asBits))
  io.outputs.valid := Delay(inputs.valid, 2, init = False)
  io.outputs.last := Delay(inputs.last, 2, init = False)
}

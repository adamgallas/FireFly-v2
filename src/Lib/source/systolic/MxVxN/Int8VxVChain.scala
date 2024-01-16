package systolic.MxVxN

import spinal.core._
import spinal.lib._
import systolic._
import xilinx.DSP48E2._
import scala.language.postfixOps

case class Int8VxVChain(length: Int, needInit: Boolean = false)
  extends AdderChain(Bits(8 bits), Vec(Bits(8 bits), 2), Vec(Bits(18 bits), 2), length) {
  require(length % 4 == 0)

  val pipe = (1, 2, 1, 1, 1)
  val dsp48s = Array.fill(length / 4)(Array(DSP48E2Build().genStaticDSP48E2(pipe, "PA=D+A/M=PAxB/P=M+C")) ++
    Array.fill(3)(DSP48E2Build().genStaticDSP48E2(pipe, "PA=D+A/M=PAxB/P=M+C+PCIN")))
  val dsp48sFlat = dsp48s.flatten

  for (i <- 0 until length / 4) {
    dsp48s(i).head.CASCDATAIN.P.clearAll()
    for (t <- 1 until 4)
      dsp48s(i)(t).CASCDATAIN.P := dsp48s(i)(t - 1).CASCDATAOUT.P
  }
  for (i <- 1 until length / 4) {
    dsp48s(i)(2).DATAIN.C := RegNext(dsp48s(i - 1).last.DATAOUT.P)
  }

  (dsp48sFlat, inputs.linked).zipped.foreach(_.DATAIN.A := _ (0).asSInt.resize(12 bits) ## B(0, 18 bits))
  (dsp48sFlat, inputs.linked).zipped.foreach(_.DATAIN.D := _ (1).asSInt.resize(27 bits).asBits)
  (dsp48sFlat, inputs.value).zipped.foreach(_.DATAIN.B := _.asSInt.resize(18 bits).asBits)
  io.outputs.fragment(0) := dsp48sFlat.last.DATAOUT.P.drop(18).take(18)
  io.outputs.fragment(1) := dsp48sFlat.last.DATAOUT.P.take(18)
  io.outputs.valid := Delay(inputs.valid, 4, init = False)
  io.outputs.last := Delay(inputs.last, 4, init = False)
}

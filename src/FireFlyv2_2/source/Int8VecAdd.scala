import spinal.core._
import spinal.lib._
import systolic._
import xilinx.DSP48E2._

import scala.language.postfixOps

case class Int8VecAdd(timeStep: Int, length: Int, needInit: Boolean = false)
  extends AdderChainAcc(
    Vec(Bits(timeStep bits), 2),
    Vec(Vec(Bits(8 bits), 4), 2),
    Vec(Vec(Bits(12 bits), 4), timeStep),
    length, needInit
  ) {

  val lastDSPAccValid = Bool()  setAsReg() init False
  val dsp48s = for (t <- 0 until timeStep) yield {
    for (i <- 0 until length) yield {
      DSP48E2Build().genDynamicALUDSP48E2(
        pipe = (1, 1, 1, 0, 0),
        enable = (
          inputs.value(i)(1)(t),
          if (i == length - 1) lastDSPAccValid else False,
          // Bool(i != 0),
          if (i == 0) False
          else if (i == length - 1) io.inputs.valid
          else True,
          inputs.value(i)(0)(t)
        ),
        mode = "4"
      )
    }
  }

  dsp48s.foreach {
    v =>
      v.last.addGeneric("IS_RSTA_INVERTED", "1'b1")
      v.last.addGeneric("IS_RSTB_INVERTED", "1'b1")
      v.last.addGeneric("IS_RSTC_INVERTED", "1'b1")
      v.last.RSTs.A.removeAssignments()
      v.last.RSTs.B.removeAssignments()
      v.last.RSTs.C.removeAssignments()
      v.last.RSTs.A := io.inputs.valid
      v.last.RSTs.B := io.inputs.valid
      v.last.RSTs.C := io.inputs.valid
  }

  val outputs = Flow(Vec(Vec(Bits(12 bits), 4), timeStep))
  val union = Vec(
    Vec(inputs.linked.map(s => Vec(s(0).map(_.asSInt.resize(12 bits))).asBits)),
    Vec(inputs.linked.map(s => Vec(s(1).map(_.asSInt.resize(12 bits))).asBits))
  )
  for (t <- 0 until timeStep) {
    for (i <- 1 until length)
      dsp48s(t)(i).CASCDATAIN.P := dsp48s(t)(i - 1).CASCDATAOUT.P
    (dsp48s(t), union(1)).zipped.foreach(_.DATAIN.C := _)
    (dsp48s(t), union(0)).zipped.foreach(_.DATAIN.A := _.drop(18))
    (dsp48s(t), union(0)).zipped.foreach(_.DATAIN.B := _.take(18))
    io.outputs.payload(t) := Vec(dsp48s(t).last.DATAOUT.P.subdivideIn(12 bits).map(_.asBits))
  }
  io.outputs.valid := Delay(inputs.last, 2, init = False)
  lastDSPAccValid.setWhen(inputs.valid).clearWhen(inputs.last)
}
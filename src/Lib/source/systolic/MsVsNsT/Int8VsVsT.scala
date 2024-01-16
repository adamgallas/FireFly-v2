package systolic.MsVsNsT

import spinal.core._
import spinal.lib._
import systolic._
import xilinx.DSP48E2._

import scala.language.postfixOps

case class Int8VsVsT(timeStep: Int, length: Int, needInit: Boolean = false)
  extends AdderChainAcc(
    Vec(Bits(timeStep bits), 2),
    Vec(Vec(Bits(8 bits), 4), 2),
    Vec(Vec(Bits(18 bits), 4), timeStep),
    length, needInit
  ) {

  val dsp48s = for (t <- 0 until timeStep) yield {
    for (i <- 0 until length) yield {
      DSP48E2Build().genDynamicALUDSP48E2(
        pipe = (1, 1, 1, 0, 0),
        enable = (inputs.value(i)(1)(t), False, Bool(i != 0), inputs.value(i)(0)(t)),
        mode = "4"
      )
    }
  }

  val outputs = Flow(Fragment(Vec(Vec(Bits(12 bits), 4), timeStep)))
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
    outputs.fragment(t) := Vec(dsp48s(t).last.DATAOUT.P.subdivideIn(12 bits).map(_.asBits))
  }
  outputs.valid := Delay(inputs.valid, 2, init = False)
  outputs.last := Delay(inputs.last, 2, init = False)

  val outputsDly0 = Flow(Fragment(Vec(Vec(Bits(12 bits), 4), timeStep)))
  outputsDly0.valid := RegNext(outputs.valid, init = False)
  outputsDly0.last := RegNext(outputs.last, init = False)
  outputsDly0.fragment := RegNext(outputs.fragment)

  val outputsDly = Flow(Fragment(Vec(Vec(Bits(12 bits), 4), timeStep)))
  outputsDly.valid := RegNext(outputsDly0.valid, init = False)
  outputsDly.last := RegNext(outputsDly0.last, init = False)
  outputsDly.fragment := RegNext(outputsDly0.fragment)

  val valids = Array.fill(timeStep)(RegNext(outputsDly0.valid, init = False) addAttribute "keep")

  val acc = Array.fill(timeStep)(Array.fill(2)(new Accumulator()))
  (acc, valids).zipped.foreach((a, v) => a.foreach(_.io.inputs.valid := v))
  acc.foreach(_.foreach(_.io.inputs.last := outputsDly.last))
  for (t <- 0 until timeStep) {
    for (i <- 0 until 2) {
      acc(t)(i).io.inputs.fragment(0) := outputsDly.fragment(t)(i * 2 + 0).asSInt.resize(24 bits).asBits
      acc(t)(i).io.inputs.fragment(1) := outputsDly.fragment(t)(i * 2 + 1).asSInt.resize(24 bits).asBits
      io.outputs.payload(t)(i * 2 + 0) := acc(t)(i).io.outputs.payload(0).resize(18 bits).asBits
      io.outputs.payload(t)(i * 2 + 1) := acc(t)(i).io.outputs.payload(1).resize(18 bits).asBits
    }
  }
  io.outputs.valid := Delay(outputsDly.last, 2, init = False)
}

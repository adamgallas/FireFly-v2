package systolic

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

case class IntAccChain(length: Int, width: Int, compensate: Boolean = false) extends Component {
  val io = new Bundle {
    val inputs = Vec(slave(Flow(Fragment(Vec(Bits(width bits), 2)))), length)
    val outputs = master(Flow(Fragment(Vec(Bits(24 bits), 2))))
  }
  noIoPrefix()
  val selOfIn, selOfPCIN, selOfP = Vec(Bool(), length)
  val dsp48s = for (i <- 0 until length) yield {
    xilinx.DSP48E2.DSP48E2Build().genDynamicALUDSP48E2(
      pipe = (1, 1, 1, 0, 0),
      enable = (selOfIn(i), selOfP(i), selOfPCIN(i), selOfIn(i)),
      mode = "2"
    )
  }

  (dsp48s, io.inputs).zipped.foreach((d, s) => d.DATAIN.C := s.fragment(1).asSInt.resize(24 bits) ## s.fragment(0).asSInt.resize(24 bits))
  if (compensate) (dsp48s, io.inputs).zipped.foreach((d, s) => d.DATAIN.CARRYIN := s.fragment(1).takeHigh(1).resized)
  for (i <- 1 until length) dsp48s(i).CASCDATAIN.P := dsp48s(i - 1).CASCDATAOUT.P

  val valids = Vec(io.inputs.map(_.valid))
  val lasts = Vec(io.inputs.map(_.last))
  val lastDlys = Range(0, length).map(i => Delay(lasts.last, i + 1, init = False))

  val shift = Vec(Bool(), length)
  for (i <- 0 until length) {
    shift(i).setAsReg().init(false)
    shift(i).setWhen(lasts.last).clearWhen(lastDlys(i))
  }

  selOfIn := valids
  selOfP := Vec(valids.map(Bool() setAsReg() init False setWhen _ clearWhen lasts.last))
  selOfPCIN := shift

  io.outputs.fragment(0) := dsp48s.last.DATAOUT.P.take(24)
  io.outputs.fragment(1) := dsp48s.last.DATAOUT.P.drop(24).take(24)
  io.outputs.valid := RegNext(shift.last, init = False)
  io.outputs.last := RegNext(lastDlys.last, init = False)
}

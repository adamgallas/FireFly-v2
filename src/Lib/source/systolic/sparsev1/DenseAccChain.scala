package systolic.sparsev1

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

case class DenseAccChain(length: Int, width: Int) extends Component {
  val io = new Bundle {
    val inputs = Vec(slave(Flow(Fragment(Vec(Bits(width bits), length)))), 2)
    val outputs = master(Flow(Fragment(Vec(Bits(24 bits), 2))))
  }
  noIoPrefix()
  val selOfInC, selOfInAB, selOfPCIN, selOfP = Vec(Bool(), length / 2)
  val dsp48s = for (i <- 0 until length / 2) yield {
    xilinx.DSP48E2.DSP48E2Build().genDynamicALUDSP48E2(
      pipe = (1, 1, 1, 0, 0),
      enable = (selOfInC(i), selOfP(i), selOfPCIN(i), selOfInAB(i)),
      mode = "2"
    )
  }

  val C = io.inputs(0).fragment.grouped(2).toArray.map(s => s(1).asSInt.resize(24 bits) ## s(0).asSInt.resize(24 bits))
  val AB = io.inputs(1).fragment.grouped(2).toArray.map(s => s(1).asSInt.resize(24 bits) ## s(0).asSInt.resize(24 bits))

  (dsp48s, C).zipped.foreach((d, s) => d.DATAIN.C := s)
  (dsp48s, AB).zipped.foreach((d, s) => d.DATAIN.A := s.takeHigh(30))
  (dsp48s, AB).zipped.foreach((d, s) => d.DATAIN.B := s.take(18))
  for (i <- 1 until length / 2) dsp48s(i).CASCDATAIN.P := dsp48s(i - 1).CASCDATAOUT.P

  selOfInC.foreach(_ := io.inputs(0).valid)
  selOfInAB.foreach(_ := io.inputs(1).valid)

  val xorLast = io.inputs(0).last ^ io.inputs(1).last
  val andLast = io.inputs(0).last & io.inputs(1).last
  val xorLastSet = Bool() setAsReg() init False toggleWhen xorLast

  val valid = io.inputs(0).valid | io.inputs(1).valid
  val last = andLast | (xorLastSet & xorLast)
  val lastDlys = Range(0, length / 2).map(i => Delay(last, i + 1, init = False))
  val keep = Bool() setAsReg() init False setWhen valid clearWhen last

  val shift = Vec(Bool(), length / 2)
  for (i <- 0 until length / 2) {
    shift(i).setAsReg().init(false)
    shift(i).setWhen(last).clearWhen(lastDlys(i))
  }

  selOfP.foreach(_ := keep)
  selOfPCIN := shift
  io.outputs.fragment(0) := dsp48s.last.DATAOUT.P.take(24)
  io.outputs.fragment(1) := dsp48s.last.DATAOUT.P.drop(24).take(24)
  io.outputs.valid := RegNext(shift.last, init = False)
  io.outputs.last := RegNext(lastDlys.last, init = False)
}

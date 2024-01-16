package systolic.MxVxN

import spinal.core._
import spinal.lib._
import systolic._
import xilinx.DSP48E2._
import scala.language.postfixOps

case class Int8MxVxNMatrixWrapper(ddrClk: ClockDomain, parallelism: (Int, Int, Int)) extends Component {
  val (m, v, n) = parallelism
  val (mr, vr, nr) = (m / 2, v / 2, n)
  val io = new Bundle {
    val inputs = slave(Flow(Fragment(utils.Linked(Vec(Vec(Bits(8 bits), v), n), Vec(Vec(Bits(8 bits), v), m)))))
    val outputs = master(Flow(Fragment(Vec(Bits(24 bits), m)))) addTag crossClockDomain
  }
  noIoPrefix()

  def m2sPipe[T <: Data](in: Flow[Fragment[T]]) = {
    val ret = Flow(Fragment(in.fragmentType)) addTag crossClockDomain
    ret.valid := RegNext(in.valid, init = False) addTag crossClockDomain
    ret.last := RegNext(in.last, init = False) addTag crossClockDomain
    ret.fragment := RegNext(in.fragment) addTag crossClockDomain
    ret
  }

  val dataType = HardType(utils.Linked(Vec(Vec(Bits(8 bits), vr), nr), Vec(Vec(Vec(Bits(8 bits), 2), vr), mr)))
  val inputs = Flow(Fragment(Vec(dataType, 2)))
  for (iv <- 0 until v) {
    for (i <- 0 until n) inputs.fragment(iv / vr).value(i)(iv % vr) := io.inputs.value(n - 1 - i)(iv)
    for (i <- 0 until mr) {
      inputs.fragment(iv / vr).linked(i)(iv % vr)(0) := io.inputs.linked(i * 2 + 0)(iv)
      inputs.fragment(iv / vr).linked(i)(iv % vr)(1) := io.inputs.linked(i * 2 + 1)(iv)
    }
  }
  inputs.valid := io.inputs.valid
  inputs.last := io.inputs.last && io.inputs.valid
  val inputsPipe = m2sPipe(inputs)

  val ddr = new ClockingArea(ddrClk) {
    val s2t = DataDoubleClockRate(dataType)
    val matrix = Int8MxVxNMatrix((mr, vr, nr))
    val align = SystolicAlign(Vec(Bits(24 bits), 2), mr)

    inputsPipe >> s2t.io.inputs
    s2t.io.outputs >> matrix.io.inputs

    val acc = Array.fill(mr)(IntAccChain(nr, 18, compensate = true))
    for (i <- 0 until mr) {
      for (j <- 0 until nr) {
        acc(i).io.inputs(j) << m2sPipe(matrix.io.outputs(j)(i))
      }
      m2sPipe(acc(i).io.outputs) >> align.io.inputs(i)
    }
  }

  val slowDown = new SlowDownBuffer(ddrClk, Vec(Bits(24 bits), m), nr, 32)

  slowDown.io.inputs.valid := ddr.align.io.outputs.valid
  slowDown.io.inputs.last := ddr.align.io.outputs.last
  slowDown.io.inputs.fragment.assignFromBits(ddr.align.io.outputs.fragment.asBits)
  io.outputs << slowDown.io.outputs
}

object Int8MxVxNMatrixWrapper {
  def main(args: Array[String]): Unit = {
    SpinalVerilog(new Int8MxVxNMatrixWrapper(ClockDomain.external("ddrClk"), (16, 16, 8)))
  }
}